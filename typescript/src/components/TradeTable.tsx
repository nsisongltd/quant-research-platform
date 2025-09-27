import React from 'react';

interface TradeData {
  id: number;
  symbol: string;
  timestamp: string;
  price: number;
  quantity: number;
  side: string;
}

interface TradeTableProps {
  trades: TradeData[];
}

const TradeTable: React.FC<TradeTableProps> = ({ trades }) => {
  return (
    <div className="bg-white p-6 rounded-lg shadow-md">
      <h3 className="text-lg font-semibold mb-4">Trade History</h3>
      <div className="overflow-x-auto">
        <table className="min-w-full table-auto">
          <thead>
            <tr className="bg-gray-50">
              <th className="px-4 py-2 text-left">ID</th>
              <th className="px-4 py-2 text-left">Symbol</th>
              <th className="px-4 py-2 text-left">Timestamp</th>
              <th className="px-4 py-2 text-left">Price</th>
              <th className="px-4 py-2 text-left">Quantity</th>
              <th className="px-4 py-2 text-left">Side</th>
              <th className="px-4 py-2 text-left">Value</th>
            </tr>
          </thead>
          <tbody>
            {trades.map((trade) => (
              <tr key={trade.id} className="border-b">
                <td className="px-4 py-2">{trade.id}</td>
                <td className="px-4 py-2 font-mono">{trade.symbol}</td>
                <td className="px-4 py-2 text-sm">{new Date(trade.timestamp).toLocaleString()}</td>
                <td className="px-4 py-2">${trade.price.toFixed(2)}</td>
                <td className="px-4 py-2">{trade.quantity}</td>
                <td className="px-4 py-2">
                  <span className={`px-2 py-1 rounded text-xs font-semibold ${
                    trade.side === 'BUY' 
                      ? 'bg-green-100 text-green-800' 
                      : 'bg-red-100 text-red-800'
                  }`}>
                    {trade.side}
                  </span>
                </td>
                <td className="px-4 py-2">${(trade.price * trade.quantity).toFixed(2)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default TradeTable;
