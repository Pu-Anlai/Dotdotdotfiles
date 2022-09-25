function line --description "Print only one line from standard input."
    sed -n {$argv[1]}p
end
