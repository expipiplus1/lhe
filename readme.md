# An implementation of Logarithmic Hop Encoding

Paper:
https://www.researchgate.net/publication/275229568_Logarithmical_hopping_encoding_A_low_computational_complexity_algorithm_for_image_compression

Note that there is an error in equation 2: The 4th case should read:
`(h_{i-1} * (255-(pred/a(x)))^(1/k(x))` (and similarly for the 5th case).
