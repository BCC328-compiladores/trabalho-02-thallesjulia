// Exemplo com struct e arrays
struct Person {
    name : string;
    age : int;
    height : float;
}

func main() : void {
    let people : Person[3];
    people[0] = Person{"Alice", 25, 1.65};
    
    let i : int = 0;
    while (i < 3) {
        i = i + 1;
    }
    
    // Operadores logicos
    if (i >= 3 && i != 0) {
        return;
    }
}
