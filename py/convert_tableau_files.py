import os
import zipfile
import pantab
import argparse
import shutil


def extract_hyper_files(twbx_dir, output_dir):
    hyper_file_info = []  # To store tuples of (hyper_file_path, twbx_base_name)
    for root, _, files in os.walk(twbx_dir):
        for file in files:
            if not file.lower().endswith(".twbx"):
                continue
            twbx_path = os.path.join(root, file)
            twbx_base_name = os.path.splitext(file)[0]
            with zipfile.ZipFile(twbx_path, "r") as zip_ref:
                hyper_files = [
                    item for item in zip_ref.namelist()
                    if item.lower().endswith(".hyper")
                ]
                for item in hyper_files:
                    hyper_file_name = os.path.basename(item)
                    # Process hyper_file_name to create subdirectory name
                    subdir_name = hyper_file_name.lower().replace(' ', '_').replace('.', '_')
                    subdir_path = os.path.join(output_dir, subdir_name)
                    os.makedirs(subdir_path, exist_ok=True)
                    # Rename hyper file to include twbx base name
                    new_hyper_file_name = f"{twbx_base_name}_{hyper_file_name}"
                    new_hyper_file_path = os.path.join(subdir_path, new_hyper_file_name)
                    # Extract hyper file directly to new_hyper_file_path
                    with zip_ref.open(item) as source, open(new_hyper_file_path, 'wb') as target:
                        shutil.copyfileobj(source, target)
                    print(f"Extracted {new_hyper_file_name} to {subdir_path}")
                    # Store the mapping
                    hyper_file_info.append((new_hyper_file_path, twbx_base_name))
    return hyper_file_info  # Return the list of hyper file paths and twbx names


def convert_hyper_to_csv(hyper_file, twbx_base_name):
    print(f"Converting {hyper_file} to .csv")
    tables = pantab.frames_from_hyper(hyper_file)
    for table_name, df in tables.items():
        if isinstance(table_name, tuple):
            filename = "_".join(table_name) + ".csv"
        else:
            filename = str(table_name) + ".csv"
        # Use the twbx base name in the CSV file name
        csv_file = f"{twbx_base_name}_{filename}"
        csv_file = (
            csv_file.replace(' ', '_')
            .replace('.', '_')
            .replace('"', '')
            .replace("'", '')
            .lower()
        )
        # Save CSV file in the same directory as hyper_file
        csv_file_path = os.path.join(os.path.dirname(hyper_file), csv_file)
        df.to_csv(csv_file_path, index=False)
        print(f"Created CSV file {csv_file_path}")


def convert_hyper_files_to_csv(hyper_file_info, delete_hyper=False):
    for hyper_file_path, twbx_base_name in hyper_file_info:
        convert_hyper_to_csv(hyper_file_path, twbx_base_name)
        if delete_hyper:
            os.remove(hyper_file_path)
            print(f"Deleted {hyper_file_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--twbx_dir",
        type=str,
        help="Directory containing Tableau .twbx files",
        default="data/cedefop_skills_ovate_oja/twbx"
    )
    parser.add_argument(
        "--output_dir",
        type=str,
        help="Output directory for converted .csv files",
        default="data/cedefop_skills_ovate_oja/csv"
    )
    parser.add_argument(
        "--overwrite_output_dir",
        action="store_true",
        help="Overwrite output directory if it exists",
        default=False
    )
    args = parser.parse_args()

    if not os.path.exists(args.output_dir):
        os.makedirs(args.output_dir)
    elif args.overwrite_output_dir:
        # Remove all files and directories in the output directory
        for root, dirs, files in os.walk(args.output_dir, topdown=False):
            for file in files:
                os.remove(os.path.join(root, file))
            for dir in dirs:
                os.rmdir(os.path.join(root, dir))
        print(f"Cleared output directory '{args.output_dir}'")
    else:
        print(f"Output directory '{args.output_dir}' already exists. Use '--overwrite_output_dir' to overwrite.")
        exit(1)

    hyper_file_info = extract_hyper_files(args.twbx_dir, args.output_dir)
    convert_hyper_files_to_csv(hyper_file_info, delete_hyper=True)
