data Date = Date Int Int Int -- year, month, day

showDate :: Date -> String
showDate (Date year month day) =
  show year ++ "-" ++ show month ++ "-" ++ show day

type Name = String

data Anniversary
  = Birthday Name Date
  | Wedding Name Name Date

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
  name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) =
  name1 ++ " married " ++ name2 ++ " on " ++ showDate date

type AnniversaryBook = [Anniversary]

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" (Date 1968 7 3)

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" (Date 1987 3 4)

anniversariesOfJohnSmith :: AnniversaryBook
anniversariesOfJohnSmith = [johnSmith, smithWedding]
