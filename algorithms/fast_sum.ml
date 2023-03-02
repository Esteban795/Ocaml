let fast_sum tbl = 
  let maxi_ending_here = ref 0 in
  let maxi_so_far = ref 0 in
    for i = 0 to ((Array.length tbl) - 1) do
        maxi_ending_here := max (!maxi_ending_here + tbl.(i)) 0;
        maxi_so_far := max !maxi_ending_here !maxi_so_far;
    done;
  (!maxi_so_far)