// Funcao identidade
func id(x) {
    return x;
}

// Funcao map generica
forall a b . func map(f : (a) -> b, v : a[]) : b[] {
    let result = new b[10];
    for (i = 0; i < 10; i = i + 1) {
        result[i] = f(v[i]);
    }
    return result;
}

func main() : void {
    let x : int = 42;
    let y : float = 3.14;
    let s : string = "hello";
    let b : bool = true;
}
