from decimal import *

# TODO: Where does this come from?
# scaling_factor = int(1)

kit = Decimal(3928478924648448718)
f_mint = Decimal(21) / Decimal(10)
liq_pen = Decimal(1) / Decimal(10)
minting_price = Decimal(1) # 18446744073709551616000000/18446744073709551616000000
collateral_at_auction = Decimal(0)

# collateral = Decimal(4369345928872593390) # Updated to not include the reward)4369345928872593390

# Note: reward appears to be rounded UP -> this is the decimal 4364976582942720796.610
collateral = Decimal(4364976582942720797) # Updated to not include the reward)

num = (
    ((kit * f_mint * minting_price) -
    ((Decimal(1)-liq_pen)*f_mint*collateral_at_auction) -
    collateral))

den = ((Decimal(1) - liq_pen)*f_mint - Decimal(1))

tez_to_auction =  (num / den)


print("Using decimal math:")
print(
    f"tez_to_auction= {num}/{den} = {tez_to_auction}"
)

# # Transaction minus fees, rounded up (cdiv)
# t = Decimal(4369345928872593390)
# t_f = ((999*t) / Decimal(1000)) + Decimal(1e6)
# t_f = t_f.to_integral_exact(rounding=ROUND_CEILING)

# k = (Decimal(89)*t + Decimal(100)*t_f + Decimal(178)) / Decimal(210)
# # 3930330695067299483.752380952
# # 3930330695067299484
# print("Expected kit to trigger condition")
# print(k)

# print("Using integer math until the end:")

# num = 18446744073709551616000000*210*3928478924648448718 - 18446744073709551616000000*100*4364976582942720797 # Updated to not include the reward
# den = 18446744073709551616000000*89
# int_tez_to_auction = Decimal(num) / Decimal(den)
# print(
#     f"tez_to_auction= {num}/{den} = {int_tez_to_auction}"
# )

# print("Using unsimplified integer math until the end:")
# k = 3928478924648448718
# t = 4364976582942720797 # Updated to not include the reward
# ta = 0
# ln = 1
# ld = 10
# pn = 18446744073709551616000000
# pd = 18446744073709551616000000
# fn = 21
# fd = 10

# num = ld*fd*(k*pn*fn*ld*fd - pd*fd*(ld-ln)*fn*ta - pd*fd*ld*fd*t)

# den = pd*fd*ld*fd*((ld-ln)*fn - ld*fd)

# quote, remainder = divmod(num, den)
# print(
#     f"tez_to_auction= {num}/{den} = {quote} r={remainder}"
# )

# print("Using simplified (v1) integer math until the end:")

# num = k*pn*fn*ld*fd - pd*fd*(ld-ln)*fn*ta - pd*fd*ld*fd*t

# den = pd*fd*((ld-ln)*fn - ld*fd)

# quote, remainder = divmod(num, den)
# print(
#     f"tez_to_auction= {num}/{den} = {quote} r={remainder}"
# )

# print("Grouped into terms for numerator")


# term1 = k*pn*fn*ld*fd
# term2 = pd*fd*(ld-ln)*fn*ta
# term3 = pd*fd*ld*fd*t

# num = term1 - term2 - term3
# den = pd*fd*((ld-ln)*fn - ld*fd)

# quote, remainder = divmod(num, den)
# print(f"term1={term1}; term2={term2}; term3={term3}")
# print(
#     f"tez_to_auction= {num}/{den} = {quote} r={remainder}"
# )

# #####################################################
# ##### For other testing, can be removed
# ################################################

# print("With nonzero colat at auction")
# k = 12788740338319672625
# t = 4341343806479347908 # Updated to not include the reward
# ta = 1571262037052642797
# ln = 1
# ld = 10
# pn = 18446744073709551616000000
# pd = 18446744073709551616000000
# fn = 21
# fd = 10

# term1 = k*pn*fn*ld*fd
# term2 = pd*fd*(ld-ln)*fn*ta
# term3 = pd*fd*ld*fd*t

# num = term1 - term2 - term3
# den = pd*fd*((ld-ln)*fn - ld*fd)

# quote, remainder = divmod(num, den)
# print(f"term1={term1}; term2={term2}; term3={term3}")
# print(
#     f"tez_to_auction= {num}/{den} = {quote} r={remainder}"
# )
