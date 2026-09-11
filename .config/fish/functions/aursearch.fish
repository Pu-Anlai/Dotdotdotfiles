function aursearch
    open "https://aur.archlinux.org/packages?O=0&K=$(string join + $argv)"
end
