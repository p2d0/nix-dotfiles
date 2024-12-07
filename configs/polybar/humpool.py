#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: with ps; [ requests beautifulsoup4 python-dotenv ])"
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta

def fetch_pending_balance():
    # URL to access account details
    url = "https://www.humpool.com/account?login=kaspa%3Aqrmvx9hm8nt5c42ajruzkj7qdlzdl3jz2jattqhl9ycvkf7jwwx6juuytjk94&coin=KAS"

    # Headers and cookies (replace as needed)
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36",
    }
    cookies = {
        # Replace with your actual session cookie if required
        "sessionid": "your_session_id_here",
    }

    # Request account page
    response = requests.get(url, headers=headers, cookies=cookies)

    if response.status_code == 200:
        soup = BeautifulSoup(response.text, "html.parser")

        # Find the pending balance
        pending_balance_element = soup.find("div", class_="card card1").find("span")
        if pending_balance_element:
            return float(pending_balance_element.text.strip())
        else:
            return None
    else:
        return None

def fetch_kas_to_rub_rate():
    # URL to fetch KAS to RUB conversion rate
    url = "https://api.coingecko.com/api/v3/simple/price?ids=kaspa&vs_currencies=rub"

    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        return data.get("kaspa", {}).get("rub")
    else:
        return None

def calculate_electricity_cost(start_datetime, elapsed_hours=None):
    # Current time
    now = datetime.now()

    # Total elapsed time in hours since the start date
    if elapsed_hours is None:
        elapsed_hours = int((now - start_datetime).total_seconds() / 3600)

    # Power consumption in kWh per hour
    power_consumption_per_hour = 0.1  # 100W = 0.1 kWh/hour

    # Time and rate percentages
    rates = [
        (0.33, 2.98),   # 33% of time, rate is 2.98 per kWh
        (0.375, 6.43),  # 37.5% of time, rate is 6.43 per kWh
        (0.291, 9.35)   # 29.1% of time, rate is 9.35 per kWh
    ]

    total_cost = 0
    for percentage, rate in rates:
        cost = elapsed_hours * power_consumption_per_hour * percentage * rate
        total_cost += cost

    return total_cost

if __name__ == "__main__":
    # Start datetime for electricity calculation
    start_datetime = datetime(2024, 12, 6, 16, 0)

    # Fetch data
    pending_balance = fetch_pending_balance()
    kas_to_rub_rate = fetch_kas_to_rub_rate()

    if pending_balance is not None and kas_to_rub_rate is not None:
        now = datetime.now()
        elapsed_days = (now - start_datetime).days
        if elapsed_days == 0:  # Avoid division by zero
            elapsed_days = 1

        pending_balance_rub = pending_balance * kas_to_rub_rate
        electricity_cost = calculate_electricity_cost(start_datetime)
        profit = pending_balance_rub - electricity_cost

        # Calculate daily averages
        daily_pending_balance = pending_balance / elapsed_days
        daily_electricity_cost = electricity_cost / elapsed_days
        daily_profit = daily_pending_balance * kas_to_rub_rate - daily_electricity_cost

        # Project yearly profit
        yearly_profit = daily_profit * 365

        print(f"KAS: {pending_balance:.3f} RUB: {pending_balance_rub:.2f} "
              f"Electricity: {electricity_cost:.2f} RUB Profit: {profit:.2f} RUB "
              f"Yearly Projected Profit: {yearly_profit:.2f} RUB")
    else:
        print("Failed to fetch data.")
