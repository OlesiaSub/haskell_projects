# Функциональное программирование II (`task06-fp-yat`)

В этом задании вам требуется написать несколько функций, работающие
с программами на искусственном языке программирования "ЯТЬ".
ЯТЬ — императивный язык программирования с переменными,
единственный тип данных — целое число,
любое выражение возвращает значение,
области видимости переменных — динамические
(а не лексические, как в популярных языках общего назначения).

Программа на языке ЯТЬ (`Program`) состоит из нескольких определений
функций (`FunctionDefinition`) с разными именами и основного тела программы (`Body`).
Любая функция может вызвать любую другую, в том числе рекурсивно.

Некоторые языки программирования различают _выражения_ (expression) и _инструкции_ (statement),
в ЯТЬ такого различия нет.
Любое выражение в ЯТЬ возвращает некоторое целочисленное значение.
Любое выражение может являться частью другого выражения.
Существует специальное выражение `Block`, которое вычисляет
несколько других выражений подряд (фигурные скобки).

Тело программы или функции — это одно выражение типа `Expression`.
Состояние интерпретатора `State` — это набор видимых в данной точке программы имён
переменных и их значений (область видимости, scope).
Scope может изменяться при выполнении выражения `Assign` (присваивание переменной).
Новые scope создаются в двух случаях: при запуске программы и
при вызове функции (выражение `FunctionScope`).

