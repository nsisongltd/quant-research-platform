import React from 'react';
import Head from 'next/head';

const Home: React.FC = () => {
  return (
    <div>
      <Head>
        <title>Quant Research Platform</title>
        <meta name="description" content="Cross-language quant research and backtesting platform" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className="container mx-auto px-4 py-8">
        <div className="text-center">
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            Quant Research Platform
          </h1>
          <p className="text-xl text-gray-600 mb-8">
            Cross-language quant research and backtesting platform
          </p>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6 max-w-4xl mx-auto">
            <div className="bg-white p-6 rounded-lg shadow-md border">
              <h2 className="text-2xl font-semibold text-blue-600 mb-2">Python</h2>
              <p className="text-gray-600">Data & Strategy Layer</p>
              <p className="text-sm text-gray-500 mt-2">
                Generate data, run backtests, ML strategies
              </p>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow-md border">
              <h2 className="text-2xl font-semibold text-purple-600 mb-2">Haskell</h2>
              <p className="text-gray-600">Validation & Rules Layer</p>
              <p className="text-sm text-gray-500 mt-2">
                Enforce correctness, prevent invalid trades
              </p>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow-md border">
              <h2 className="text-2xl font-semibold text-green-600 mb-2">TypeScript</h2>
              <p className="text-gray-600">Dashboard & UI Layer</p>
              <p className="text-sm text-gray-500 mt-2">
                Visualize results, interactive charts
              </p>
            </div>
          </div>
          
          <div className="mt-12 p-6 bg-gray-50 rounded-lg">
            <h3 className="text-2xl font-semibold mb-4">Development Status</h3>
            <div className="text-left max-w-2xl mx-auto">
              <div className="flex items-center mb-2">
                <span className="w-3 h-3 bg-green-500 rounded-full mr-3"></span>
                <span>Day 1: Trade data pipeline established</span>
              </div>
              <div className="flex items-center mb-2">
                <span className="w-3 h-3 bg-yellow-500 rounded-full mr-3"></span>
                <span>Day 2: Strategy simulation (planned)</span>
              </div>
              <div className="flex items-center">
                <span className="w-3 h-3 bg-gray-300 rounded-full mr-3"></span>
                <span>Day 3+: Dashboard visualization (planned)</span>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
};

export default Home;
