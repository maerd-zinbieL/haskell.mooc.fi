#/usr/bin/zsh
if [ $# -ne 1 ]
then echo "输入一个文件名"
else stack ghci $1
fi
