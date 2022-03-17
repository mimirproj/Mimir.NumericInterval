module Mimir.NumericInterval

open Mimir.FSharp.Extensions
open System.Diagnostics

[<RequireQualifiedAccess>]        
[<NoComparison; NoEquality>]
type Bound<'n> =
    | Inclusive of 'n
    | Exclusive of 'n
    
type 'n bound = Bound<'n> 
    
[<RequireQualifiedAccess>]        
module Bound =
    let inline asInclusiveValue isLeft bound =
        match bound with
        | Bound.Inclusive n ->
            n
            
        | Bound.Exclusive n ->
            if isLeft then
                n + LanguagePrimitives.GenericOne
            else
                n - LanguagePrimitives.GenericOne
                
    let inline asExclusiveValue isLeft bound =
        match bound with
        | Bound.Inclusive n ->
            if isLeft then
                n - LanguagePrimitives.GenericOne
            else
                n + LanguagePrimitives.GenericOne
                
        | Bound.Exclusive n ->
            n
            
[<AutoOpen>]
module BoundOperators =
    let inline incl (value:'n) =
        Bound.Inclusive value
        
    let inline excl (value:'n) =
        Bound.Exclusive value



[<RequireQualifiedAccess>]
[<NoComparison; NoEquality>]
[<DebuggerDisplay("Interval {ToString()}")>]
type Interval<'n> =
    private
    | LeftClosed of 'n bound
    | RightClosed of 'n bound
    | Closed of 'n bound * 'n bound
    
    override this.ToString() =
        let mapBound isLeft = function
            | Bound.Inclusive n -> 
                if isLeft then ('[', n) else (']', n)
                    
            | Bound.Exclusive n -> 
                if isLeft then ('(', n) else (')', n)
                    
        match this with
        | Interval.LeftClosed l ->
            let (cl, nl) = mapBound true l
            sprintf "%c%A,∞%c" cl nl ')'
            
        | Interval.RightClosed r ->
            let (cr, nr) = mapBound false r
            sprintf "%c∞,%A%c" '(' nr cr
            
        | Interval.Closed(l, r) ->
            let (cl, nl) = mapBound true l
            let (cr, nr) = mapBound false r
            sprintf "%c%A,%A%c" cl nl nr cr
            
            
[<RequireQualifiedAccess>]
module Interval =
    let leftClosed left =
        Interval.LeftClosed left
        
    let rightClosed right =
        Interval.RightClosed right
        
    let closed left right =
        let il = Bound.asInclusiveValue true left
        let ir = Bound.asInclusiveValue false right
        
        if il <= ir then
            Interval.Closed(left, right)
        else
            failwith "A closed interval's left bound should be <= the right bound!"

    let tryCreate (anyLeftBound:'n bound option) (anyRightBound:'n bound option) =
        match anyLeftBound, anyRightBound with
        | None, None ->
            None
            
        | Some left, None ->
            Some (Interval.LeftClosed left)
            
        | None, Some right ->
            Some (Interval.RightClosed right)
            
        | Some left, Some right ->
            let il = Bound.asInclusiveValue true left
            let ir = Bound.asInclusiveValue false right
            
            if il <= ir then
                Some (Interval.Closed(left, right))
            else
                None
                
    let getValues = function
        | Interval.LeftClosed left -> (Some left, None)
        | Interval.RightClosed right -> (None, Some right)
        | Interval.Closed(left, right) -> (Some right, Some left)
                
    let toSeq (interval:Interval<'n>) =
        let gen il ir =
            seq {
                let mutable iv = il 

                yield iv
                while iv < ir do
                    iv <- iv + LanguagePrimitives.GenericOne
                    yield iv
            }
            
        match interval with
        | Interval.LeftClosed left ->
            gen (Bound.asInclusiveValue true left) (maxValue())
            
        | Interval.RightClosed right ->
            gen (minValue()) (Bound.asInclusiveValue false right)
            
        | Interval.Closed(left, right) ->
            let il = Bound.asInclusiveValue true left
            let ir = Bound.asInclusiveValue false right
            
            gen il ir
            
