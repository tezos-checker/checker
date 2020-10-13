#!/usr/bin/python3

# CONSTANTS
# #############################################################################

CREATION_DEPOSIT = 1    # in TEZ
F = 2                   # dimensionless
COL_LIQ_REWARD = 0.001  # dimensionless (percentage of tez) # TODO: Use cNp

# Some Types:
#   q                # in (1/KIT)
#   tz_minting       # in TEZ
#   tz_liquidation   # in TEZ
#
#   c = collateral   # in TEZ
#   k = kit_balance  # in KIT

def kit_liquidation_limit(q, c, tz_liquidation):
  return c / (F * q * tz_liquidation)
  # Type: TEZ / (() * (1/KIT) * TEZ) = KIT

def liquidation_reward(c):
  return CREATION_DEPOSIT + COL_LIQ_REWARD * c
  # Type: TEZ + ((%TEZ) * TEZ) = TEZ

def kit_to_write_off(q, k, c, tz_liquidation, tz_minting):
  return (k * F * q * tz_liquidation - c) / (F * q * tz_liquidation - tz_minting)
  # TODO: Check that the types make sense here.

def tez_to_auction(q, k, c, tz_liquidation, tz_minting):
  return kit_to_write_off(q, k, c, tz_liquidation, tz_minting) * tz_minting
  # TODO: Check that the types make sense here.

# ENTRY POINT
# #############################################################################

if __name__ == "__main__":
  q = 1.015
  k = 20
  c = 10
  tz_liquidation = 0.32
  tz_minting = 0.36

  liquidation_limit = kit_liquidation_limit(q, c, tz_liquidation)

  if k > liquidation_limit:
    print ("Overburrowed          : True")

    reward = liquidation_reward(c)
    print ("Reward                : {0}".format(reward))

    c_without_reward = c - reward
    print ("New collateral        : {0}".format(c_without_reward))

    new_liquidation_limit = kit_liquidation_limit(q, c_without_reward, tz_liquidation)
    print ("New liquidation limit : {0}".format(new_liquidation_limit))

    amount_of_kit_to_write_off = kit_to_write_off(q, k, c_without_reward, tz_liquidation, tz_minting)
    print ("Kits to write off     : {0}".format(amount_of_kit_to_write_off))

    amount_of_tez_to_auction = tez_to_auction(q, k, c_without_reward, tz_liquidation, tz_minting)
    print ("Tez to auction        : {0}".format(amount_of_tez_to_auction))

    final_collateral = c_without_reward - amount_of_tez_to_auction
    print ("New collateral        : {0}".format(final_collateral))

    final_kit = k - amount_of_kit_to_write_off
    print ("New outstanding kit   : {0}".format(final_kit))

    final_liquidation_limit = kit_liquidation_limit(q, final_collateral, tz_liquidation)
    print ("New liquidation limit : {0}".format(final_liquidation_limit))

    print ("Still overburrowed    : {0}".format(final_kit > final_liquidation_limit))

