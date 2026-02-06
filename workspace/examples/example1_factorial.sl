// Exemplo 1: Calculo de Fatorial
func factorial(n : int) : int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main() : int {
    let result : int = factorial(6);
    print(result);
    return 0;
}
