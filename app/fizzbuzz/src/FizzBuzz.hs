-- FizzBuzz.hs - メインモジュール（エントリーポイント）

module FizzBuzz (
  -- Domain Types
  module FizzBuzz.Domain.Type.FizzBuzzType,
  module FizzBuzz.Domain.Type.FizzBuzzError,
  
  -- Domain Models
  module FizzBuzz.Domain.Model.FizzBuzzValue,
  module FizzBuzz.Domain.Model.FizzBuzzList,
  
  -- Domain Services
  module FizzBuzz.Domain.Generator,
  
  -- Application Services
  module FizzBuzz.Application.Command,
  
  -- Legacy functions for compatibility
  generate
) where

import FizzBuzz.Domain.Type.FizzBuzzType
import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue
import FizzBuzz.Domain.Model.FizzBuzzList
import FizzBuzz.Domain.Generator
import FizzBuzz.Application.Command

-- 後方互換性のため既存のgenerate関数を保持
generate :: FizzBuzzType -> Int -> String
generate fizzbuzzType num = getValue $ generateValue fizzbuzzType num