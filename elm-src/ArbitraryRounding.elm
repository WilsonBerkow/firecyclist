-- (c) Wilson Berkow

module ArbitraryRounding where

-- round by any Int, not just powers of ten
-- Example:  Rounding 48 to the NEAREST 13 gives 52 (Cuz 52's the mult of 13 closest to 48).
--           In code, this is: (arb_round 13 48) == 52
-- Note: in every version before this, I've said the answer is 39, which is such a
--  dumbass mistake, I'll apologize to the ppl who've read these comments even tho
--  it's only me. Sorry. That was f*cking stupid.

-- In these fns, `round_num` is the number whose multiples are being rounded to, and `n` is
--  the number being rounded.

arb_round_down round_num n = n - (n % round_num)
arb_round_up round_num n = arb_round_down round_num (n + round_num)
arb_round round_num n = let r_up   = arb_round_up round_num n
                            r_down = arb_round_down round_num n
                        in if r_up - n <= n - r_down
                            then r_up
                            else r_down