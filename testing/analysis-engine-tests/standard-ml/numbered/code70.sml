let exception e of 'a
in (raise e(5)) handle e(f) => f(7)
end;
exception e' of 'a;
e 1
