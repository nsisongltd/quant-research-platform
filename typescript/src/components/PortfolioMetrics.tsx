import React from 'react';

interface TradeData {
  id: number;
  symbol: string;
  timestamp: string;
  price: number;
  quantity: number;
  side: string;
}

interface PortfolioMetricsProps {
  trades: TradeData[];
}

const PortfolioMetrics: React.FC<PortfolioMetricsProps> = ({ trades }) => {
  const calculateMetrics = () => {
    let cash = 10000;
    let totalTrades = trades.length;
    let buyTrades = trades.filter(t => t.side === 'BUY').length;
    let sellTrades = trades.filter(t => t.side === 'SELL').length;
    
    trades.forEach(trade => {
      const tradeValue = trade.price * trade.quantity;
      if (trade.side === 'BUY') {
        cash -= tradeValue;
      } else {
        cash += tradeValue;
      }
    });

    const totalVolume = trades.reduce((sum, trade) => sum + (trade.price * trade.quantity), 0);
    const avgTradeSize = totalVolume / totalTrades;
    const returnPercent = ((cash - 10000) / 10000) * 100;

    return {
      finalCash: cash,
      totalTrades,
      buyTrades,
      sellTrades,
      totalVolume,
      avgTradeSize,
      returnPercent
    };
  };

  const metrics = calculateMetrics();

  return (
    <div className="bg-white p-6 rounded-lg shadow-md">
      <h3 className="text-lg font-semibold mb-4">Portfolio Metrics</h3>
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <div className="text-center">
          <div className="text-2xl font-bold text-blue-600">${metrics.finalCash.toFixed(2)}</div>
          <div className="text-sm text-gray-600">Final Cash</div>
        </div>
        <div className="text-center">
          <div className="text-2xl font-bold text-green-600">{metrics.totalTrades}</div>
          <div className="text-sm text-gray-600">Total Trades</div>
        </div>
        <div className="text-center">
          <div className="text-2xl font-bold text-purple-600">{metrics.buyTrades}</div>
          <div className="text-sm text-gray-600">Buy Trades</div>
        </div>
        <div className="text-center">
          <div className="text-2xl font-bold text-red-600">{metrics.sellTrades}</div>
          <div className="text-sm text-gray-600">Sell Trades</div>
        </div>
        <div className="text-center">
          <div className="text-2xl font-bold text-indigo-600">${metrics.totalVolume.toFixed(2)}</div>
          <div className="text-sm text-gray-600">Total Volume</div>
        </div>
        <div className="text-center">
          <div className="text-2xl font-bold text-pink-600">${metrics.avgTradeSize.toFixed(2)}</div>
          <div className="text-sm text-gray-600">Avg Trade Size</div>
        </div>
        <div className="text-center">
          <div className={`text-2xl font-bold ${metrics.returnPercent >= 0 ? 'text-green-600' : 'text-red-600'}`}>
            {metrics.returnPercent.toFixed(2)}%
          </div>
          <div className="text-sm text-gray-600">Return</div>
        </div>
      </div>
    </div>
  );
};

export default PortfolioMetrics;
