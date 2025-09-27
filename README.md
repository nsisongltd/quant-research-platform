# Quant Research Platform

I intend to build a quant research and backtesting platform using Python, Haskell, and TypeScript together. The idea is to code daily, growing the platform piece by piece until it becomes a solid project for quants.

---

## Vision
The project is a cross-language quant research system:
- **Python** → Data & Strategy Layer  
  - Fetch/generate data, run backtests, test ML-driven strategies.
- **Haskell** → Validation & Rules Layer  
  - Enforce correctness, prevent invalid trades, risk checks, and guarantee no bad logic passes through.
- **TypeScript** → Dashboard & UI Layer  
  - Visualize results, strategies, and backtests with an interactive web dashboard.

**The philosophy is:** 
- **Python for flexibility** (quick strategy experiments).  
- **Haskell for safety** (make it impossible to run with bad data).  
- **TypeScript for clarity** (see results in a clean UI).  

---

## Tech Stack
- Python 3.10+ (pandas, numpy, matplotlib).  
- Haskell (GHC + Cabal/Stack, with `cassava` CSV library).  
- TypeScript (Next.js, React, Recharts for visualization).  

---

## Documentation
- Check releases to see versions of what I've done
- I would make sure to keep daily commits small and descriptive (since i have decided it'd be open source).  

---