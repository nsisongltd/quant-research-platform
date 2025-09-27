import { NextApiRequest, NextApiResponse } from 'next';
import fs from 'fs';
import path from 'path';
import csv from 'csv-parser';

interface TradeData {
  id: number;
  symbol: string;
  timestamp: string;
  price: number;
  quantity: number;
  side: string;
}

export default function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'GET') {
    return res.status(405).json({ message: 'Method not allowed' });
  }

  const csvPath = path.join(process.cwd(), '..', 'python', 'data', 'strategy_trades.csv');
  
  if (!fs.existsSync(csvPath)) {
    return res.status(404).json({ message: 'Trade data not found' });
  }

  const trades: TradeData[] = [];

  fs.createReadStream(csvPath)
    .pipe(csv())
    .on('data', (row) => {
      trades.push({
        id: parseInt(row.id),
        symbol: row.symbol,
        timestamp: row.timestamp,
        price: parseFloat(row.price),
        quantity: parseInt(row.quantity),
        side: row.side
      });
    })
    .on('end', () => {
      res.status(200).json(trades);
    })
    .on('error', (error) => {
      console.error('Error reading CSV:', error);
      res.status(500).json({ message: 'Error reading trade data' });
    });
}
