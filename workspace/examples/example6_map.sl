// Exemplo 6: Funcao map generica
forall a b . func map(f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (i = 0; i < v.size; i = i + 1) {
        result[i] = f(v[i]);
    }
    return result;
}

func main() : void {
    print(0);
}
