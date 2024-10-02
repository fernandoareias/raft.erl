#!/bin/bash

# Verifica se o argumento foi fornecido
if [ $# -ne 1 ]; then
    echo "Uso: $0 <valor>"
    exit 1
fi

# Valor fornecido como argumento
valor="$1"

# Diretório binário
bin_dir="bin"

# Comando Erlang
erl_command="erl -eval 'process:start($valor).' -noshell"

# Acessa o diretório binário
cd "$bin_dir" || exit

# Executa o comando Erlang
eval "$erl_command"

# Retorna ao diretório original
cd - || exit
