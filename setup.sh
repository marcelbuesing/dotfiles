# Clone repo
# git@github.com:marcelbuesing/dotfiles.git ~/dotfiles

# Setup Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
ln -s ~/dotfiles/.zshrc ~/.zshrc

# Setup Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

ln -s ~/dotfiles/.spacemacs ~/.spacemacs
ln -s ~/dotfiles/spacemacs-personal/mymain/ ~/.emacs.d/private/
