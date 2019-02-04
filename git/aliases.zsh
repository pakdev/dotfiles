#!/bin/sh
alias gl='git log'
alias glc="git log --graph --decorate --oneline --abbrev-commit"
alias gp='git push origin HEAD'
alias gpa='git push origin --all'
alias gpom='git pull origin master'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch -v'
alias ga='git add'
alias gaa='git add -A'
alias gau='git add -u'
alias gcm='git commit -m'
alias gcam='git commit -a -m'
alias gs='git status'
alias glnext='git log --oneline $(git describe --tags --abbrev=0 @^)..@'

grs() {
    back=$*
    re='^[0-9]+$'
    if ! [[ $back =~ $re ]]; then
        echo "error: number of commits not specified" >&2
    fi
    git reset --soft "HEAD~$back"
}

gi() {
	curl -s "https://www.gitignore.io/api/$*"
}
