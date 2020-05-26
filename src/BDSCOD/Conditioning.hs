module BDSCOD.Conditioning where

import BDSCOD.Llhd
import Epidemic.Types


-- | The probability that a single individual does not give rise to any
-- observable event during a period of time.
--
-- This can be used to condition on there being at least one observation since
-- the origin time.
probabilityUnobserved ::
  (Rate,Rate,Rate)  -- ^ birth, death and observation rate
  -> Time           -- ^ duration of period
  -> Probability    -- ^ probability no observed event
probabilityUnobserved (l, m, obsRate) duration = p0 dummyParams duration 1
  where
    dummyParams = (l, m, obsRate, [], 0, [])
