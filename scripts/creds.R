# Interactive Script for Getting Credentials

# Source this File in your Script/Project and Enter Your Password

# Encrypted Keys
here_key <- 'LTO327bpgA0lzw9HRYCrCRinxS8GdP6kGqkxELhRWzn3PIY9RKegmoZZWuuz1loPjJ2cjE14/9/qRmTsqn/XSlAD3EJMdL8F4b6/D2BzCx4b0yM='
google_key <- 'oUjEUI6qSpK3OuD4P5fJZpnJD1IpQ/kUhDjwhX+E8mMkafvmNs2sm9QlVInv1G3oxRepsU5AL7rb808owmHEpIGSHFM/xrmoz6eNNthdOA=='

# Get Password and Decrypt
k <- sodium::sha256(charToRaw(getPass::getPass('Password:')))
key <- cyphr::key_sodium(k)

here_key <- cyphr::decrypt_string(base64enc::base64decode(here_key), key)
google_key <- cyphr::decrypt_string(base64enc::base64decode(google_key), key)

rm(k, key)

# Encryption Process

# encrypt <- function(api_key){
#   k <- sodium::sha256(charToRaw(getPass::getPass('Password:\n')))
#   key <- cyphr::key_sodium(k)
#   raw_enc <- cyphr::encrypt_string(api_key, key)
#   base64 <- base64enc::base64encode(raw_enc)
#   return(base64)
# }
