#!/bin/bash

# Nome do diretório de destino
bin_dir="bin"

rm -R "$bin_dir"

# Criar o diretório se não existir
mkdir -p "$bin_dir"

# Lista de arquivos de origem Erlang para compilar
modules="process.erl raft_state.erl log_manager.erl"

# Compilar cada módulo e mover os arquivos .beam para o diretório binário
for module in $modules; do
    erlc -o "$bin_dir" "$module"
done
