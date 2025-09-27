import csv
import random
from datetime import datetime, timedelta
import os

def generate_trade_data(num_trades=100, output_file="data/trades.csv"):
    symbols = ["AAPL", "TSLA", "MSFT", "BTC-USD"]
    
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    rows = []
    base_date = datetime.now() - timedelta(days=30)
    
    for i in range(num_trades):
        trade = {
            "id": i + 1,
            "symbol": random.choice(symbols),
            "timestamp": (base_date + timedelta(minutes=i*15)).isoformat(),
            "price": round(random.uniform(100, 500), 2),
            "quantity": random.randint(1, 10),
            "side": random.choice(["BUY", "SELL"]),
        }
        rows.append(trade)
    
    with open(output_file, "w", newline="") as f:
        if rows:
            writer = csv.DictWriter(f, fieldnames=rows[0].keys())
            writer.writeheader()
            writer.writerows(rows)
    
    print(f"generated {num_trades} trades in {output_file}")
    return output_file

def main():
    print("quant research platform - python layer")
    print("=" * 50)
    
    output_file = generate_trade_data()
    print(f"trade data ready for haskell validation: {output_file}")

if __name__ == "__main__":
    main()