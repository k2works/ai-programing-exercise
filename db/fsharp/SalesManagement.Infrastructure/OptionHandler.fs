namespace SalesManagement.Infrastructure

open System
open System.Data
open Dapper

/// <summary>
/// Dapper 用の F# Option 型 TypeHandler
/// </summary>
type OptionHandler<'T>() =
    inherit SqlMapper.TypeHandler<'T option>()

    override _.SetValue(param: IDbDataParameter, value: 'T option) =
        match value with
        | Some v ->
            param.Value <- box v
        | None ->
            param.Value <- DBNull.Value

    override _.Parse(value: obj) : 'T option =
        if isNull value || value = box DBNull.Value then
            None
        else
            Some (unbox value)

/// <summary>
/// Option TypeHandler を登録
/// </summary>
module OptionHandlerRegistration =
    let private registered = ref false

    let register () =
        if not !registered then
            SqlMapper.AddTypeHandler(OptionHandler<DateTime>())
            SqlMapper.AddTypeHandler(OptionHandler<string>())
            SqlMapper.AddTypeHandler(OptionHandler<int>())
            SqlMapper.AddTypeHandler(OptionHandler<int64>())
            SqlMapper.AddTypeHandler(OptionHandler<decimal>())
            registered := true
