function pyfart
    set venvdir (mktemp --tmpdir -d pyfart-XXX)
    python -m venv "$venvdir"
    source "$venvdir"/bin/activate.fish
end
