##-----Merge 2 Sorted vectors into 1 sorted vector-----
# Prepare test data
a = sort(sample(1:1000, 100))
b = sort(sample(1:1000, 100))

# Initialize indices
a_ix = 1 # input 1
b_ix = 1 # input 2
c_ix = 1 # output

c = rep(0,length(a) + length(b)) # Initialize ouput to all zeros

while ( (a_ix <= length(a)) & (b_ix <= length(b)) ){ # Fill in c, until one index runs out
  if (a[a_ix] < b[b_ix]){                            # If the next element of a is smaller
    c[c_ix] = a[a_ix]                                # - then add a-element to c
    c_ix  = c_ix + 1                                 # - Increment the index of c
    a_ix = a_ix +1                                   # - Increment the index of a
  } else {                                           # Else (next element of b is smaller)
    c[c_ix] = b[b_ix]                                # - then add b-element to c
    c_ix  = c_ix + 1                                 # - Increment the index of c
    b_ix = b_ix +1                                   # - Increment the index of b
  }
  
}

# Since a or b might be different lengths, we need to check and finish off c
while( (a_ix <= length(a)) ){ # Check if any a left
  c[c_ix] = a[a_ix]
  c_ix  = c_ix + 1
  a_ix = a_ix +1
}

while( (b_ix <= length(b)) ){ # Check if any b left
  c[c_ix] = b[b_ix]
  c_ix  = c_ix + 1
  b_ix = b_ix +1
}
