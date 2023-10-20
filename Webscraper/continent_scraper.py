"""Script to scrape country continent data from statisticstimes.com
before running ensure that the correct Chrome Driver Binaries are installed and accessible (set by constant `CHROME_DRIVER`)
see: https://chromedriver.chromium.org/downloads
"""

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from dataclasses import dataclass
import csv

#CONSTANTS
CONTINENTS = ["Asia", "Africa", "Europe", "North America", "South America", "Oceania"]
OUTPUT_DIR = "./country_codes_and_continents.csv"
WEBSITE = "https://statisticstimes.com/geography/countries-by-continents.php"
CHROME_DRIVER = './chromedriver.exe'

#CLASSES
@dataclass
class Country:
    number: int
    name: str
    code: str
    m49: int
    region_and_sub_region: str
    continent: str

#FUNCTIONS
def find_index_of_code(country_info: list[str]) -> int:
    """Loops through country strings and returns the index of code, E.G: 'NED' """
    for i, info in enumerate(country_info):
        if len(info) == 3 and info.isupper():
            return i
    return -1
        
def find_indexes_of_continent(country_info: list[str]) -> list[int]:
    if country_info[-2] + " " + country_info[-1] in CONTINENTS:
        return [-2, -1]
    return [-1]

def get_country_continent_by_indexes(country_entry: list[str], continent_indexes: list[int]) -> str:
    country_continent = ""
    for index in continent_indexes:
        country_continent += country_entry[index] + " "
    return country_continent.rstrip()

def get_country_name_by_code_index(country_entry: list[str], code_index: int) -> str:
    country_name = ""
    for i in range(1, code_index):
        country_name += country_entry[i] + " "
    return country_name.rstrip()

def get_country_region_sub_region_by_surrounding_indexes(country_entry: list[str], code_index: int, continent_indexes: list[int]) -> str:
    entry_len = len(country_entry)
    left_fence = code_index + 2
    if len(continent_indexes) == 2:  right_fence = entry_len -2
    else: right_fence = entry_len -1
    country_region_sub_region = ""
    for i in range(left_fence, right_fence):
        country_region_sub_region += country_entry[i] + " "
    return country_region_sub_region.rstrip()

def save_data(path: str, data: list[Country]) -> None:
    with open(path, 'w', newline='') as f:
        writer = csv.writer(f, delimiter=',')
        writer.writerow(['number', 'name', 'code', 'm49', 'region_and_sub_region', 'continent'])
        for country in data:
            writer.writerow([country.number, country.name, country.code, country.m49, country.region_and_sub_region, country.continent])

def merge_data(odd_data: list[Country], even_data: list[Country]) -> list[Country]:
    odd_amount = len(odd_data)
    even_amount = len(even_data)
    odd_index = 0
    even_index = 0
    merged_list = []
    while odd_index < odd_amount or even_index < even_amount:
        if odd_index < odd_amount:
            merged_list.append(odd_data[odd_index])
            odd_index += 1
        if even_index < even_amount:
            merged_list.append(even_data[even_index])
            even_index += 1
    return merged_list

def generate_dataset(country_entries: list[str]) -> list[Country]:
    """
    Example country entry: "156 Netherlands NLD 528 Western Europe Europe"
    Extraction:
    number  name         code   m49 region   subregion  continent
    156     Netherlands  NLD    528 Western  Europe     Europe
    """
    country_list : list[Country] = []
    for entry in country_entries:
        entry_text = entry.text.split()
        code_index = find_index_of_code(entry_text )
        if code_index == -1: continue
        country_name = get_country_name_by_code_index(entry_text , code_index)
        continent_indexes = find_indexes_of_continent(entry_text)
        country_continent = get_country_continent_by_indexes(entry_text, continent_indexes)
        #country region and sub region are all indexes between the code and continent indexes
        country_region_and_sub_region = get_country_region_sub_region_by_surrounding_indexes(entry_text, code_index, continent_indexes)
        country_list.append(Country(entry_text[0], country_name, entry_text[code_index], entry_text[code_index + 1], country_region_and_sub_region, country_continent))

    return country_list

if __name__ == "__main__":
    service = Service(executable_path=CHROME_DRIVER)
    options = webdriver.ChromeOptions()
    options.headless = True
    driver = webdriver.Chrome(service=service, options=options)
    driver.get(WEBSITE)
    odd_entries = driver.find_elements(By.CLASS_NAME, "odd")
    even_entries = driver.find_elements(By.CLASS_NAME, "even")
    odd_data = generate_dataset(odd_entries)
    even_data = generate_dataset(even_entries)
    merged_data = merge_data(odd_data, even_data)
    save_data(OUTPUT_DIR, merged_data)