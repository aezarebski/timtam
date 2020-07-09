module BDSCOD.Conditioning where

import BDSCOD.Llhd
import Epidemic.Types.Parameter


-- | The /probability/ that a single individual does not give rise to any
-- observable event during a period of time.
--
-- __NOTE__ The rates here assume the total birth, death and observation rates.
-- If there are multiple ways in which the process can be observed then the
-- total observation rate is the sum of their rates.
probabilityUnobserved ::
  (Rate,Rate,Rate)  -- ^ birth, death and observation rate
  -> Time           -- ^ duration of period
  -> Probability    -- ^ probability no observed event
probabilityUnobserved (l, m, obsRate) duration = p0 dummyParams duration 1
  where
    dummyParams = (l, m, obsRate, [], 0, [])
