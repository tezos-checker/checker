#!/usr/bin/env python

import math as math

def next_drift(drift, p, last_t, current_t):
    return drift + (0.5 *
                    (drift + rate_of_change_of_drift(p)) *
                    (current_t - last_t))

def next_quantity(q, drift, next_drift, rate_of_change_drift, last_t, current_t):
    time_difference = current_t - last_t
    return q * math.exp(
        (drift + (
            (1.0 / 6.0) * (2.0 * drift + next_drift) * time_difference)) *
        time_difference)

def rate_of_change_of_drift(p):
    def sign_of_log(val):
        if val < 1.0:
            return -1.0
        if val > 1.0:
            return 1.0
        return 0.0
    if p < math.exp(0.005):
        return 0.0
    if p < math.exp(0.05):
        return sign_of_log(p) * 0.01
    return sign_of_log(p) * 0.05

def target_price_in_tez(q, tz, k):
    p = (q * tz) / k
    return p

def protected_index(last_protected_index, tz, interval):
    epsilon = 0.0005 # 0.05 cNp/min
    min_value = math.exp(- epsilon * interval)
    max_value = math.exp( epsilon * interval)
    return last_protected_index * max(min(tz/last_protected_index, max_value), min_value)
