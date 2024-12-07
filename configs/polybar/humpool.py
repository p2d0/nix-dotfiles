#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: with ps; [ requests beautifulsoup4 python-dotenv ])"

from dotenv import load_dotenv
import requests
from bs4 import BeautifulSoup

def fetch_pending_balance():
    # Replace with the appropriate URL and session token
    url = "https://www.humpool.com/account?login=kaspa%3Aqrmvx9hm8nt5c42ajruzkj7qdlzdl3jz2jattqhl9ycvkf7jwwx6juuytjk94&coin=KAS"

    # If authentication is required, set cookies or headers
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36",
    }

    response = requests.get(url, headers=headers,)

    # Check for a successful response
    if response.status_code == 200:
        soup = BeautifulSoup(response.text, "html.parser")

        # Find the pending balance by locating the appropriate element
        # This selector is based on the provided HTML snippet
        pending_balance_element = soup.find("div", class_="card card1").find("span")
        if pending_balance_element:
            pending_balance = pending_balance_element.text.strip()
            print(f"{pending_balance} KAS")
            return pending_balance
        else:
            print("Pending balance not found.")
    else:
        print(f"Failed to fetch data. HTTP Status Code: {response.status_code}")

if __name__ == "__main__":
    fetch_pending_balance()
