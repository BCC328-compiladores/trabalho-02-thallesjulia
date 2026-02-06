#!/bin/bash
# Script para testar o compilador SL em todos os arquivos de exemplo
# Compilador SL - BCC328 - Construcao de Compiladores I

set -o pipefail

# Contadores globais
total=0
passed=0
failed=0

run_test() {
    local phase=$1
    local file=$2
    local name
    name=$(basename "$file")
    ((total++))

    local output
    output=$(cabal run -v0 slc -- "--${phase}" "$file" 2>&1)
    local rc=$?

    if [ $rc -eq 0 ]; then
        echo "  [OK] ${name}"
        ((passed++))
    else
        echo "  [FAIL] ${name}"
        echo "$output" | head -5 | sed 's/^/        /'
        ((failed++))
    fi
}

# Garante que o projeto compila antes de testar
echo "+============================================+"
echo "|   Testes do Compilador SL - BCC328         |"
echo "+============================================+"
echo ""

echo "Compilando projeto..."
if ! cabal build -v0 2>&1; then
    echo "[FAIL] Falha na compilacao. Abortando testes."
    exit 1
fi
echo "[OK] Compilacao bem-sucedida"
echo ""

# Descobre arquivos de exemplo
shopt -s nullglob
example_files=(examples/*.sl)

if [ ${#example_files[@]} -eq 0 ]; then
    echo "[FAIL] Nenhum arquivo .sl encontrado em examples/"
    exit 1
fi

echo "Arquivos de exemplo: ${#example_files[@]}"
for file in "${example_files[@]}"; do
    echo "  - $(basename "$file")"
done

# Fase 1: Analise Lexica
echo ""
echo "=== Analise Lexica (--lexer) ==="
for file in "${example_files[@]}"; do
    run_test "lexer" "$file"
done

# Fase 2: Analise Sintatica
echo ""
echo "=== Analise Sintatica (--parser) ==="
for file in "${example_files[@]}"; do
    run_test "parser" "$file"
done

# Fase 3: Pretty Printer
echo ""
echo "=== Pretty Printer (--pretty) ==="
for file in "${example_files[@]}"; do
    run_test "pretty" "$file"
done

# Fase 4: Analise Semantica
echo ""
echo "=== Analise Semantica (--check) ==="
for file in "${example_files[@]}"; do
    run_test "check" "$file"
done

# Fase 5: Interpretador
echo ""
echo "=== Interpretador (--run) ==="
for file in "${example_files[@]}"; do
    run_test "run" "$file"
done

# Fase 6: Testes Unitarios
echo ""
echo "=== Testes Unitarios (cabal test) ==="
echo ""
cabal test 2>&1
test_exit_code=$?

if [ $test_exit_code -eq 0 ]; then
    echo "  [OK] Suite de testes unitarios"
    ((passed++))
else
    echo "  [FAIL] Suite de testes unitarios"
    ((failed++))
fi
((total++))

# Resumo
echo ""
echo "============================================"
echo "TOTAL: ${passed}/${total} OK, ${failed} FALHAS"
echo "============================================"

if [ $failed -eq 0 ] && [ $test_exit_code -eq 0 ]; then
    echo "[OK] Todos os testes passaram!"
    exit 0
else
    echo "[FAIL] Alguns testes falharam!"
    exit 1
fi
