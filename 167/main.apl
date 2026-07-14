{
  target = nums[⍺] + nums[⍵] : ⍺ ⍵ ⋄
  target < nums[⍺] + nums[⍵] : ⍺ ∇ (⍵-1) ⋄
  target > nums[⍺] + nums[⍵] : (⍺+1) ∇ ⍵
}
