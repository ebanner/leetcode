s←⎕
keys values←'IVXLCDM'(1 5 10 50 100 500 1000)
nums←values[keys⍳s]
⎕←+/nums×¯1*2</nums,0
