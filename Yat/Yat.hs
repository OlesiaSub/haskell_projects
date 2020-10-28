module Yat where  -- Вспомогательная строчка, чтобы можно было использовать функции в других файлах.
import Data.List
import Data.Maybe
import Data.Bifunctor
import Debug.Trace

-- В логических операциях 0 считается ложью, всё остальное - истиной.
-- При этом все логические операции могут вернуть только 0 или 1.

-- Все возможные бинарные операции: сложение, умножение, вычитание, деление, взятие по модулю, <, <=, >, >=, ==, !=, логическое &&, логическое ||
data Binop = Add | Mul | Sub | Div | Mod | Lt | Le | Gt | Ge | Eq | Ne | And | Or

-- Все возможные унарные операции: смена знака числа и логическое "не".
data Unop = Neg | Not

data Expression = Number Integer  -- Возвращает число, побочных эффектов нет.
                | Reference Name  -- Возвращает значение соответствующей переменной в текущем scope, побочных эффектов нет.
                | Assign Name Expression  -- Вычисляет операнд, а потом изменяет значение соответствующей переменной и возвращает его. Если соответствующей переменной нет, она создаётся.
                | BinaryOperation Binop Expression Expression  -- Вычисляет сначала левый операнд, потом правый, потом возвращает результат операции. Других побочных эффектов нет.
                | UnaryOperation Unop Expression  -- Вычисляет операнд, потом применяет операцию и возвращает результат. Других побочных эффектов нет.
                | FunctionCall Name [Expression]  -- Вычисляет аргументы от первого к последнему в текущем scope, потом создаёт новый scope для дочерней функции (копию текущего с добавленными параметрами), возвращает результат работы функции.
                | Conditional Expression Expression Expression -- Вычисляет первый Expression, в случае истины вычисляет второй Expression, в случае лжи - третий. Возвращает соответствующее вычисленное значение.
                | Block [Expression] -- Вычисляет в текущем scope все выражения по очереди от первого к последнему, результат вычисления -- это результат вычисления последнего выражения или 0, если список пуст.

type Name = String
type FunctionDefinition = (Name, [Name], Expression)  -- Имя функции, имена параметров, тело функции
type State = [(String, Integer)]  -- Список пар (имя переменной, значение). Новые значения дописываются в начало, а не перезаписываютсpя
type Program = ([FunctionDefinition], Expression)  -- Все объявленные функций и основное тело программы

showBinop :: Binop -> String
showBinop Add = "+"
showBinop Mul = "*"
showBinop Sub = "-"
showBinop Div = "/"
showBinop Mod = "%"
showBinop Lt  = "<"
showBinop Le  = "<="
showBinop Gt  = ">"
showBinop Ge  = ">="
showBinop Eq  = "=="
showBinop Ne  = "/="
showBinop And = "&&"
showBinop Or  = "||"

showUnop :: Unop -> String
showUnop Neg = "-"
showUnop Not = "!"

showExpr :: Expression -> String
showExpr (Number num)                      = show num
showExpr (Reference name)                  = name
showExpr (Assign name ex)                  = "let " ++ name ++ " = " ++ showExpr ex ++ " tel"
showExpr (BinaryOperation oper fir sec)    = "(" ++ showExpr fir ++ " " ++ showBinop oper ++ " " ++ showExpr sec ++ ")"
showExpr (UnaryOperation oper ex)          = showUnop oper ++ showExpr ex
showExpr (FunctionCall name [])            = name ++ "()"
showExpr (FunctionCall name ex)            = name ++ "(" ++ intercalate ", " (map showExpr ex) ++ ")"
showExpr (Conditional ex t f)              = "if " ++ showExpr ex ++ " then " ++ showExpr t ++ " else " ++ showExpr f ++ " fi"
showExpr (Block [])                        = "{\n}"
showExpr (Block ex)                        = "{\n" ++ intercalate ";\n" (map (intercalate "\n" . map ("\t"++) . lines . showExpr) ex) ++ "\n}"

showFunc :: FunctionDefinition -> String
showFunc (name, args, ex) = "func " ++ name ++ "(" ++ intercalate ", " args ++ ") = " ++ showExpr ex

-- Верните текстовое представление программы (см. условие).

showProgram :: Program -> String
showProgram (funcs, ex) = concatMap ((++ "\n") . showFunc) funcs ++ showExpr ex

toBool :: Integer -> Bool
toBool = (/=) 0

fromBool :: Bool -> Integer
fromBool False = 0
fromBool True  = 1

toBinaryFunction :: Binop -> Integer -> Integer -> Integer
toBinaryFunction Add = (+)
toBinaryFunction Mul = (*)
toBinaryFunction Sub = (-)
toBinaryFunction Div = div
toBinaryFunction Mod = mod
toBinaryFunction Lt  = (.) fromBool . (<)
toBinaryFunction Le  = (.) fromBool . (<=)
toBinaryFunction Gt  = (.) fromBool . (>)
toBinaryFunction Ge  = (.) fromBool . (>=)
toBinaryFunction Eq  = (.) fromBool . (==)
toBinaryFunction Ne  = (.) fromBool . (/=)
toBinaryFunction And = \l r -> fromBool $ toBool l && toBool r
toBinaryFunction Or  = \l r -> fromBool $ toBool l || toBool r

toUnaryFunction :: Unop -> Integer -> Integer
toUnaryFunction Neg = negate
toUnaryFunction Not = fromBool . not . toBool

getName:: FunctionDefinition -> Name
getName (name, _, _) = name

getArgs :: FunctionDefinition -> ([Name], Expression)
getArgs (_, args, fun) = (args, fun)

getDef::[FunctionDefinition] -> Name -> ([Name], Expression)
getDef f n = fun (head (filter (equal n) f))
                where equal n f = n == getName f
                      fun       = getArgs

--getBody :: FunctionDefinition -> Expression
--getBody (_, _, expr) = expr

getValue :: State -> Name -> Integer
getValue [] _                = 0
getValue ((n, v):scope) name | name == n = v
                             | otherwise = getValue scope name

addScope::State -> [Name] -> [Integer] -> State
addScope scope name val = zip name val ++ scope

chainFuncs::[FunctionDefinition] -> State -> [Expression] -> ([Integer], State)
chainFuncs funcs scope []         = ([], scope)
chainFuncs funcs scope (ex : es)  = (fst x : fst xs, snd xs)
                                 where x  = evalExpression funcs scope ex
                                       xs = chainFuncs funcs (snd x) es

-- changed order

evalExpression :: [FunctionDefinition] -> State -> Expression -> (Integer, State)
evalExpression funcs scope (Number num)                 = (num, scope)

evalExpression funcs scope (Reference ref)              = (getValue scope ref, scope)

evalExpression funcs scope (Assign name expr)           = (fst val, var : snd val)
                                                                 where val = evalExpression funcs scope expr
                                                                       var = (name, fst val)

--simplify

evalExpression funcs scope (BinaryOperation op fir sec) = first (toBinaryFunction op (fst tmp_l)) tmp_r
                                                            where tmp_l = evalExpression funcs scope fir
                                                                  tmp_r = evalExpression funcs (snd tmp_l) sec

evalExpression funcs scope (UnaryOperation uop op)      = first (toUnaryFunction uop) calc
                                                            where calc = evalExpression funcs scope op

evalExpression funcs scope (FunctionCall name ex)        = (ans, snd newScope)
                                                            where ans            = fst (evalExpression funcs tempScope (snd fun))
                                                                  fun            = getDef funcs name
                                                                  newScope       = chainFuncs funcs scope ex
                                                                  tempScope      = addScope (snd newScope) (fst fun) (fst newScope)

evalExpression funcs scope (Conditional cond fir sec)   | toBool(fst condition) = evalExpression funcs (snd condition) fir
                                                        | otherwise             = evalExpression funcs (snd condition) sec
                                                            where condition     = evalExpression funcs scope cond

evalExpression funcs scope (Block [])                   = (0, scope)
evalExpression funcs scope (Block [x])                  = evalExpression funcs scope x
evalExpression funcs scope (Block (x:xs))               = evalExpression funcs (snd (evalExpression funcs scope x)) (Block xs)

eval :: Program -> Integer
eval (funcs, expr) = fst (evalExpression funcs [] expr)
