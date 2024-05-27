import os
import zipfile
import pantab
import argparse


def extract_hyper_files(twbx_dir, output_dir):
  for root, _, files in os.walk(twbx_dir):
    for file in files:
      if not file.lower().endswith(".twbx"):
        continue
      twbx_path = os.path.join(root, file)
      with zipfile.ZipFile(twbx_path, "r") as zip_ref:
        for item in zip_ref.namelist():
          if item.startswith("Data/") and item.lower().endswith(".hyper"):
            # extract only .hyper file without directory structure, rename to match twbx file
            hyper_file = os.path.basename(item)
            zip_ref.extract(item, output_dir)
            os.rename(os.path.join(output_dir, item), os.path.join(output_dir, os.path.splitext(file)[0] + ".hyper"))
            # remove lefover dirs
            # os.rmdir(os.path.join(output_dir, "Data"))            


def convert_hyper_to_csv(hyper_file):
  print(f"Converting {hyper_file} to .csv")
  tables = pantab.frames_from_hyper(hyper_file)
  for table_name, df in tables.items():
    if isinstance(table_name, tuple):
      filename = "_".join(table_name) + ".csv"
    else:
      filename = str(table_name) + ".csv"
    csv_file = os.path.splitext(hyper_file)[0] + "_" + filename
    csv_file = csv_file.replace(' ', '_').replace('.', '_').replace('"', '').lower()
    df.to_csv(csv_file, index=False)


def convert_hyper_files_to_csv(hyper_dir, delete_hyper=False):
  for root, _, files in os.walk(hyper_dir):
    for file in files:
      if not file.lower().endswith(".hyper"):
        continue
      hyper_file = os.path.join(root, file)
      convert_hyper_to_csv(hyper_file)
      if delete_hyper:
        os.remove(hyper_file)


if __name__ == "__main__":
  parser = argparse.ArgumentParser()
  parser.add_argument(
    "--twbx_dir", type=str, #required=True, 
    help="Directory containing Tableau .twbx files", 
    default="data/cedefop_skills_ovate/twbx"
  )
  parser.add_argument("--output_dir", 
    type=str, #required=True, 
    help="Output directory for converted .csv files",
    default="data/cedefop_skills_ovate/csv"
  )
  parser.add_argument(
    "--overwrite_output_dir",
    action="store_true", 
    help="Overwrite output directory if it exists",
    default=True
  )
  args = parser.parse_args()
  
  if not os.path.exists(args.output_dir):
    os.makedirs(args.output_dir)
  elif args.overwrite_output_dir:
    for root, _, files in os.walk(args.output_dir):
      for file in files:
        os.remove(os.path.join(root, file))
  
  extract_hyper_files(args.twbx_dir, args.output_dir)
  convert_hyper_files_to_csv(args.output_dir, delete_hyper=True)