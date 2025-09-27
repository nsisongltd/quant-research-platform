import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import os

def generate_price_data(symbol, days=30, initial_price=100):
    np.random.seed(42)
    dates = pd.date_range(start=datetime.now() - timedelta(days=days), periods=days*24, freq='H')
    
    returns = np.random.normal(0, 0.02, len(dates))
    prices = [initial_price]
    
    for ret in returns[1:]:
        new_price = prices[-1] * (1 + ret)
        prices.append(max(new_price, 1.0))
    
    return pd.DataFrame({
        'timestamp': dates,
        'symbol': symbol,
        'price': prices
    })

def calculate_sma(prices, window):
    return prices.rolling(window=window).mean()

def sma_crossover_strategy(price_data, short_window=5, long_window=20):
    price_data = price_data.copy()
    price_data['sma_short'] = calculate_sma(price_data['price'], short_window)
    price_data['sma_long'] = calculate_sma(price_data['price'], long_window)
    
    price_data['signal'] = 0
    price_data['position'] = 0
    
    for i in range(1, len(price_data)):
        if price_data['sma_short'].iloc[i] > price_data['sma_long'].iloc[i] and \
           price_data['sma_short'].iloc[i-1] <= price_data['sma_long'].iloc[i-1]:
            price_data.loc[price_data.index[i], 'signal'] = 1
        elif price_data['sma_short'].iloc[i] < price_data['sma_long'].iloc[i] and \
             price_data['sma_short'].iloc[i-1] >= price_data['sma_long'].iloc[i-1]:
            price_data.loc[price_data.index[i], 'signal'] = -1
    
    position = 0
    for i in range(len(price_data)):
        if price_data['signal'].iloc[i] == 1:
            position = 1
        elif price_data['signal'].iloc[i] == -1:
            position = 0
        price_data.loc[price_data.index[i], 'position'] = position
    
    return price_data

def generate_strategy_trades(price_data, initial_capital=10000):
    trades = []
    capital = initial_capital
    position = 0
    trade_id = 1
    
    for i in range(1, len(price_data)):
        current_price = price_data['price'].iloc[i]
        current_signal = price_data['signal'].iloc[i]
        
        if current_signal == 1 and position == 0:
            quantity = int(capital / current_price)
            if quantity > 0:
                trades.append({
                    'id': trade_id,
                    'symbol': price_data['symbol'].iloc[i],
                    'timestamp': price_data['timestamp'].iloc[i].isoformat(),
                    'price': current_price,
                    'quantity': quantity,
                    'side': 'BUY'
                })
                capital -= quantity * current_price
                position = quantity
                trade_id += 1
                
        elif current_signal == -1 and position > 0:
            trades.append({
                'id': trade_id,
                'symbol': price_data['symbol'].iloc[i],
                'timestamp': price_data['timestamp'].iloc[i].isoformat(),
                'price': current_price,
                'quantity': position,
                'side': 'SELL'
            })
            capital += position * current_price
            position = 0
            trade_id += 1
    
    return trades

def run_sma_strategy(symbol="AAPL", output_file="data/strategy_trades.csv"):
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    price_data = generate_price_data(symbol)
    strategy_data = sma_crossover_strategy(price_data)
    trades = generate_strategy_trades(strategy_data)
    
    if trades:
        df = pd.DataFrame(trades)
        df.to_csv(output_file, index=False)
        print(f"generated {len(trades)} strategy trades for {symbol}")
        print(f"strategy data saved to {output_file}")
        
        final_capital = 10000
        for trade in trades:
            if trade['side'] == 'BUY':
                final_capital -= trade['quantity'] * trade['price']
            else:
                final_capital += trade['quantity'] * trade['price']
        
        print(f"final capital: ${final_capital:.2f}")
        return output_file
    else:
        print("no trades generated")
        return None
