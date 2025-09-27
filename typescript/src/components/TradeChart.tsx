import React from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';

interface TradeData {
  id: number;
  symbol: string;
  timestamp: string;
  price: number;
  quantity: number;
  side: string;
}

interface TradeChartProps {
  trades: TradeData[];
}

const TradeChart: React.FC<TradeChartProps> = ({ trades }) => {
  const chartData = trades.map((trade, index) => ({
    time: new Date(trade.timestamp).toLocaleDateString(),
    price: trade.price,
    quantity: trade.quantity,
    side: trade.side,
    tradeId: trade.id
  }));

  return (
    <div className="bg-white p-6 rounded-lg shadow-md">
      <h3 className="text-lg font-semibold mb-4">Trade Price Chart</h3>
      <ResponsiveContainer width="100%" height={300}>
        <LineChart data={chartData}>
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis dataKey="time" />
          <YAxis />
          <Tooltip 
            formatter={(value, name) => [
              name === 'price' ? `$${value.toFixed(2)}` : value,
              name === 'price' ? 'Price' : 'Quantity'
            ]}
            labelFormatter={(label) => `Time: ${label}`}
          />
          <Line 
            type="monotone" 
            dataKey="price" 
            stroke="#8884d8" 
            strokeWidth={2}
            dot={{ fill: '#8884d8', strokeWidth: 2, r: 4 }}
          />
        </LineChart>
      </ResponsiveContainer>
    </div>
  );
};

export default TradeChart;
