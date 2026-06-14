nums←⎕
target←⎕
n←≢nums
mat←(0.5×(⍳n)∘.=⍳n)+nums∘.+nums
⎕←⊃1-⍨⍸target=mat
