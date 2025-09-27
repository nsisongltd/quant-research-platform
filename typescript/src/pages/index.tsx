import React, { useState, useEffect } from 'react';
import Head from 'next/head';
import TradeChart from '../components/TradeChart';
import PortfolioMetrics from '../components/PortfolioMetrics';
import TradeTable from '../components/TradeTable';

interface TradeData {
  id: number;
  symbol: string;
  timestamp: string;
  price: number;
  quantity: number;
  side: string;
}

const Home: React.FC = () => {
  const [trades, setTrades] = useState<TradeData[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch('/api/trades')
      .then(res => res.json())
      .then(data => {
        setTrades(data);
        setLoading(false);
      })
      .catch(() => {
        setLoading(false);
      });
  }, []);

  return (
    <div>
      <Head>
        <title>Quant Research Platform</title>
        <meta name="description" content="Cross-language quant research and backtesting platform" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className="container mx-auto px-4 py-8">
        <div className="text-center mb-8">
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            Quant Research Platform
          </h1>
          <p className="text-xl text-gray-600">
            Cross-language quant research and backtesting platform
          </p>
        </div>

        {loading ? (
          <div className="text-center py-8">
            <div className="text-lg text-gray-600">Loading trade data...</div>
          </div>
        ) : trades.length > 0 ? (
          <div className="space-y-6">
            <PortfolioMetrics trades={trades} />
            <TradeChart trades={trades} />
            <TradeTable trades={trades} />
          </div>
        ) : (
          <div className="text-center py-8">
            <div className="text-lg text-gray-600 mb-4">No trade data available</div>
            <div className="text-sm text-gray-500">
              Run the Python strategy to generate trade data
            </div>
          </div>
        )}

        <div className="mt-12 p-6 bg-gray-50 rounded-lg">
          <h3 className="text-2xl font-semibold mb-4">Development Status</h3>
          <div className="text-left max-w-2xl mx-auto">
            <div className="flex items-center mb-2">
              <span className="w-3 h-3 bg-green-500 rounded-full mr-3"></span>
              <span>Day 1: Trade data pipeline established</span>
            </div>
            <div className="flex items-center mb-2">
              <span className="w-3 h-3 bg-green-500 rounded-full mr-3"></span>
              <span>Day 2: SMA crossover strategy implemented</span>
            </div>
            <div className="flex items-center mb-2">
              <span className="w-3 h-3 bg-green-500 rounded-full mr-3"></span>
              <span>Day 3: Risk management rules added</span>
            </div>
            <div className="flex items-center">
              <span className="w-3 h-3 bg-yellow-500 rounded-full mr-3"></span>
              <span>Day 4: Dashboard visualization (in progress)</span>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
};

export default Home;
