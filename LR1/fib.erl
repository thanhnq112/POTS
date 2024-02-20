-module(fib).
-export([fib_p/1, fib_g/1, tail_fib/1]).

% Функция без хвостовой рекурсии с использованием сравнения с образцом
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) when N > 1 -> fib_p(N - 1) + fib_p(N - 2).

% Функция без хвостовой рекурсии с использованием сторожевых последовательностей
fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) when N > 1 -> fib_g(N - 1) + fib_g(N - 2).

% Функция с хвостовой рекурсией
tail_fib(N) -> tail_fib(N, 0, 1).

tail_fib(0, A, _) -> A;
tail_fib(N, A, B) -> tail_fib(N - 1, B, A + B).