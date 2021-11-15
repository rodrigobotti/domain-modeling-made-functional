type Person = { First: string; Last: string }

let aPerson = { First = "Rodrigo"; Last = "Botti" }
let { First = first; Last = last } = aPerson
let aFirst = aPerson.First
let aLast = aPerson.Last

type OrderQuantity =
  | UnitQuantity of int
  | KilogramQuantity of decimal

let aUnitQuantity = UnitQuantity 10
let aKilogramQuantity = KilogramQuantity 2.5m

let printQuantity (qty: OrderQuantity) : unit =
  match qty with
  | UnitQuantity unit -> printfn "%i units" unit
  | KilogramQuantity kg -> printfn "%g kg" kg

printQuantity aUnitQuantity
printQuantity aKilogramQuantity

// ---

type CheckNumber = CheckNumber of int
type CardNumber = CardNumber of string

type CardType =
  | Visa
  | MasterCard

type CreditCardInfo =
  { CardType: CardType
    CardNumber: CardNumber }

type PaymentMethod =
  | Cash
  | Check of CheckNumber
  | Card of CreditCardInfo

type PaymentAmount = PaymentAmount of decimal

type Currency =
  | EUR
  | USD

type Payment =
  { Amount: PaymentAmount
    Currency: Currency
    Method: PaymentMethod }

// type PayInvoice =
//   UnpaidInvoice -> Payment -> PaidInvoice
type ConvertPaymentCurrency = Payment -> Currency -> Payment

// ---

// already in F#
// type Option<'a> =
//   | Some of 'a
//   | None

type PersonalName =
  { FirstName: string
    MiddleInitial: Option<string>
    LastName: string }

type PersonalName2 =
  { FirstName: string
    MiddleInitial: string option // <- more common notation
    LastName: string }

// already in F# 4.1+
// type Result<'Success,'Failure> =
//   | OK of 'Success
//   | Error of 'Failure

// type PayInvoice =
//   UnpaidInvoice -> Payment -> Result<PaidInvoice, PaymentError>

type PaymentError =
  | CardTypeNotRecognized
  | PaymentRejected
  | PaymentProviderOffline

// list -> fixed-size immutable linked list
// array -> fixed-size mutable; elements can be fetched and assigned by index
// ResizeArray -> variable size array; F# alias to C# List<T>
// seq -> lazy collection; alias to C# IEnumerable<T>

// type Order = {
//   OrderId: OrderId
//   Lines: OrderLine list
// }

let aList = [ 1; 2; 3 ]
let aNewList = 0 :: aList

let printList aList =
  match aList with
  | [] -> printfn "list is empty"
  | [ x ] -> printfn "list has one element %A" x
  | [ x; y ] -> printfn "list has two elements %A and %A" x y
  | _longerList -> printfn "list has more than two elements"

let printList2 aList =
  match aList with
  | [] -> printfn "list is empty"
  | first :: _rest -> printfn "list is non-empty with first element %A" first

// ---

// exn == exception type
// during modeling phase while you still don't know the internals of all domain types
type Undefined = exn
type CustomerInfo = Undefined
type ShippingAddress = Undefined
// ...

type UnvalidatedOrder = Undefined
type ValidatedOrder = Undefined

type ValidationError =
  { FieldName: string
    ErrorDescription: string }

type ValidateOrderSync = UnvalidatedOrder -> Result<ValidatedOrder, ValidationError list> // Result for describing error effects
type ValidateOrderAsync = UnvalidatedOrder -> Async<Result<ValidatedOrder, ValidationError list>> // Async for describing async effects
type ValidationResponse<'a> = Async<Result<'a, ValidationError list>>
type ValidateOrder = UnvalidatedOrder -> ValidationResponse<ValidatedOrder>

// ---
(*
F# applies structural equality
i.e. record types are equal when all fields are equal
sum types are equal if they have the same choice case with same data
consequence: value objects already have derived equality
*)

// adding identity to sum types:
// approach 1 - "outside" case definitions

// data wihtout id
type UnpaidInvoideInfo1 = Undefined
type PaidInvoiceInfo1 = Undefined

// sum without id
type InvoiceInfo1 =
  | Unpaid of UnpaidInvoideInfo1
  | Paid of PaidInvoiceInfo1

// id
type InvoiceId = Undefined

// top-level invoice type
type Invoice1 =
  { InvoiceId: InvoiceId
    InvoiceInfo: InvoiceInfo1 }

// approach 2 - "inside" case definitions

type UnpaidInvoice = { InvoiceId: InvoiceId }
type PaidInvoice = { InvoiceId: InvoiceId }

type Invoice =
  | Unpaid of UnpaidInvoice
  | Paid of PaidInvoice

// preferable: all data accessible in one place when pattern matching
let invoice =
  Paid { InvoiceId = InvoiceId "some-uuid-v4-value" }

match invoice with
| Unpaid unpaid -> printfn $"The unpaid invoiceId is {unpaid.InvoiceId}"
| Paid paid -> printfn $"The paid invoiceId is {paid.InvoiceId}"

(*
implementing equality for entities:

aprroach 1 - OOD
1. override Equals method
2. override GetHashCode method
3. add CustomEquality and NoComparison attributes
*)
type Email = Email of string
type PhoneNumber = PhoneNumber of string
type ContactId = ContactId of int

[<CustomEquality; NoComparison>]
type Contact =
  { ContactId: ContactId
    PhoneNumber: PhoneNumber
    Email: Email }
  override this.Equals(obj) =
    match obj with
    | :? Contact as c -> this.ContactId = c.ContactId
    | _ -> false

  override this.GetHashCode() = hash this.ContactId

let contact1 =
  { ContactId = ContactId 1
    PhoneNumber = PhoneNumber "123-456-7890"
    Email = Email "bob@example.com" }

let contact2 =
  { ContactId = ContactId 1
    PhoneNumber = PhoneNumber "123-456-7890"
    Email = Email "robert@example.com" }

printfn $"{contact1 = contact2}" // true -> because both have same ContactId

(*
approach 2 - remove equality
preferable
removes ambiguity about equality at the object level
forces us to be explicit
*)
[<NoEquality; NoComparison>]
type Contact2 =
  { ContactId: ContactId
    PhoneNumber: PhoneNumber
    Email: Email }

let contact21: Contact2 =
  { ContactId = ContactId 1
    PhoneNumber = PhoneNumber "123-456-7890"
    Email = Email "bob@example.com" }

let contact22: Contact2 =
  { ContactId = ContactId 1
    PhoneNumber = PhoneNumber "123-456-7890"
    Email = Email "robert@example.com" }

// printfn $"{contact21 = contact22}" // -> compilation error: "type Contact2 does not support equality constraint ..."
printfn "%b" (contact21.ContactId = contact22.ContactId)

// case where multiple fields form the identity -> expose synthetic
type ProductId = ProductId of int
type OrderId = OrderId of int

type OderLine =
  { OrderId: OrderId
    ProductId: ProductId
    Qty: int }
  member this.Key() = (this.OrderId, this.ProductId) // compare using ol1.Key = ol2.Key

// updating records: copy + replace
let updatedContact1 =
  { contact1 with PhoneNumber = PhoneNumber "0000" }
