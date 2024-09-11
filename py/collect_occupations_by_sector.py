import os
import argparse
import requests
import re
from bs4 import BeautifulSoup
from datetime import datetime
import time
import csv


eu_countries = [
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
  "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
  "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"
]


sectors = {
  "05.13" : {"name": "Administrative services", "nace2_code": "N"}, # Section N
  "05.17" : {"name": "Arts & recreation and other services", "nace2_code": "R"}, # Section R
  "05.11" : {"name": "Finance & insurance", "nace2_code": "K"}, # Section K
  "05.10" : {"name": "ICT services", "nace2_code": "J"}, # Section J
  "05.12" : {"name": "Professional services", "nace2_code": "M"}, # Section M
  "03.06" : {"name": "Construction", "nace2_code": "F"}, # Section F
  "04.09" : {"name": "Accommodation & food", "nace2_code": "I"}, # Section I
  "04.08" : {"name": "Transport & storage", "nace2_code": "H"}, # Section H
  "04.07" : {"name": "Wholesale & retail trade", "nace2_code": "G"}, # Section G
  "02.05" : {"name": "Manufacturing", "nace2_code": "C"}, # Section C
  "06.15" : {"name": "Education", "nace2_code": "P"}, # Section P
  "06.14" : {"name": "Health & social care", "nace2_code": "Q"}, # Section Q
  "06.16" : {"name": "Public sector & defence", "nace2_code": "O"}, # Section O
  "01.01" : {"name": "Agriculture, forestry & fishing", "nace2_code": "A"}, # Section A
  "01.03" : {"name": "Energy supply services", "nace2_code": "D"}, # Section D
  "01.02" : {"name": "Mining & quarrying", "nace2_code": "B"}, # Section B
  "01.04" : {"name": "Water and waste treatment", "nace2_code": "E"} # Section E
}


occupations = {
  "3" : "Associate professionals",
  "3.32" : "Health associate professionals",
  "3.35" : "ICT technicians",
  "3.34" : "Legal & social associate professionals",
  "3.33" : "Office associate professionals",
  "3.31" : "Science & engineering technicians",
  "4" : "Clerks",
  "4.43" : "Accounting clerks",
  "4.42" : "Customer clerks",
  "4.41" : "Office clerks",
  "4.44" : "Other support clerks",
  "9" : "Elementary workers",
  "9.92" : "Agricultural labourers",
  "9.91" : "Cleaners and helpers",
  "9.94" : "Food preparation helpers",
  "9.96" : "Other elementary workers",
  "9.95" : "Street services workers",
  "9.93" : "Technical labourers",
  "6" : "Farm and related workers",
  "6.61" : "Farmworkers and gardeners",
  "6.62" : "Forest & fishery workers",
  "1" : "Managers",
  "1.12" : "Business managers",
  "1.11" : "CEOs, officials & legislators",
  "1.14" : "Hospitality & retail managers",
  "1.13" : "Technical managers",
  "8" : "Operators and assemblers",
  "8.82" : "Assemblers",
  "8.83" : "Drivers & vehicle operators",
  "8.81" : "Machine & plant operators",
  "2" : "Professionals",
  "2.22" : "Health professionals",
  "2.25" : "ICT professionals",
  "2.26" : "Legal & social professionals",
  "2.24" : "Office professionals",
  "2.21" : "Researchers & engineers",
  "2.23" : "Teaching professionals",
  "5" : "Service and sales workers",
  "5.53" : "Care workers",
  "5.51" : "Personal service workers",
  "5.54" : "Protection workers",
  "5.52" : "Sales workers",
  "7" : "Trades workers",
  "7.71" : "Construction workers",
  "7.74" : "Electroengineering workers",
  "7.73" : "Handicraft & printing workers",
  "7.72" : "Metal & machinery workers",
  "7.75" : "Other manufacturing workers"
}


def get_isco_code(occupation):
  for code, name in occupations.items():
    if occupation.lower() == name.lower():
      return code
  return None


def fetch_sectoral_data(country_code, sector_code):
  base_url = f'https://www.cedefop.europa.eu/dsense/render/treemap?indicator=900_sec&aes_x=occupation&unit=number&year=recent&sector={sector_code}&country={country_code}&dimensionsCombination=country+X+year+X+sector+X+occupation'
  
  headers = {
    'authority': 'www.cedefop.europa.eu',
    'accept': 'application/json, text/plain, */*',
    'accept-language': 'en-GB,en;q=0.9,en-US;q=0.8,bg;q=0.7',
    'cookie': 'cookie-agreed-version=1.0.0; cookie-agreed=2; dashboard-agreed=1',
    'referer': 'https://www.cedefop.europa.eu/',
    'sec-ch-ua': '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"Linux"',
    'sec-fetch-dest': 'empty',
    'sec-fetch-mode': 'cors',
    'sec-fetch-site': 'same-origin',
    'user-agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0'
  }
  
  response = requests.get(base_url, headers=headers)
  response.raise_for_status()  
  
  soup = BeautifulSoup(response.text, 'xml')
  
  data = []
  for title in soup.find_all('title'):
    content = title.string.strip()
    if ':' in content: # title contains occupation and number of people
      occupation, n = content.rsplit(':', 1)
      occupation = occupation.strip()
      n = n.strip()
      
      # Remove category description (e.g., "in Managers")
      occupation = re.sub(r'\s+in\s+[^:]+', '', occupation)
      
      # Remove any text within parentheses and warning messages
      occupation = re.sub(r'\s*\([^)]*\)', '', occupation)
      occupation = occupation.strip()
      
      # Remove warning messages from the number part
      n = re.sub(r'\s*\(\([^)]*\)\).*', '', n)
      
      isco_code = get_isco_code(occupation)
            
      data.append({
        "isco_code": isco_code,
        "occupation": occupation,
        "n": n,
        "sector_name": sectors[sector_code]["name"],
        "nace_rev2_code": sectors[sector_code]["nace2_code"],
        "country_code": country_code
      })

  return data


def fetch_all_sectoral_data(countries, output_file):
  all_data = []
  total_requests = len(countries) * len(sectors)
  current_request = 0

  with open(output_file, 'w', newline='', encoding='utf-8') as csvfile:
    fieldnames = ['sector_name', 'nace_rev2_code', 'occupation_name', 'occupation_code', 'country_code', 'n']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    for country in countries:
      for sector_code in sectors:
        current_request += 1
        print(f"Processing request {current_request}/{total_requests}: Country {country}, Sector {sectors[sector_code]['name']}")
        
        try:
          data = fetch_sectoral_data(country, sector_code)
          for item in data:
            writer.writerow({
              'sector_name': item['sector_name'],
              'nace_rev2_code': item['nace_rev2_code'],
              'occupation_name': item['occupation'],
              'occupation_code': item['isco_code'],
              'country_code': item['country_code'],
              'n': item['n']
            })
          all_data.extend(data)
        except Exception as e:
          print(f"Error processing country {country}, sector {sector_code}: {str(e)}")
        
        time.sleep(0.1)  # Be nice to the server

    return all_data
  

def main():
  parser = argparse.ArgumentParser(description="Fetch sectoral employment data from Cedefop")
  parser.add_argument("--countries", nargs="+", default=eu_countries, help="List of country codes to fetch data for")
  parser.add_argument("--output", default="data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv", help="Output CSV file name")
  args = parser.parse_args()

  start_time = datetime.now()
  print(f"Starting data collection at {start_time}")

  fetch_all_sectoral_data(args.countries, args.output)

  end_time = datetime.now()
  print(f"Data collection completed at {end_time}")
  print(f"Total time taken: {end_time - start_time}")
  print(f"Data saved to {args.output}")


if __name__ == "__main__":
  main()