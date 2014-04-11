call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

set background=dark
let g:solarized_termtrans=1
let g:solarized_termcolors=256
let g:solarized_contrast="high"
let g:solarized_visibility="high"
colorscheme solarized

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif


if has("syntax")
  syntax on
endif

if has("vms")
  set nobackup " do not keep a backup file, use versions instead
else
  set backup   " keep a backup file
endif

set history=50 " keep 50 lines of command line history
set ruler      " show the cursor position all the time
set showcmd    " display incomplete commands
set incsearch  " do incremental searching
set showmatch  " Show matching brackets.
set ignorecase " Do case insensitive matching
set smartcase  " Do smart case matching
set autowrite  " Automatically save before commands like :next and :make
set hidden     " Hide buffers when they are abandoned
"---set mouse=a    " Enable mouse usage (all modes)
set paste      " set pase by default to allow for nice clean pasteing

" cleaner backup/tmp file maanagement.... kinda
set backupdir=~/.backup
set directory=~/.backup


" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

else

  "set autoindent   " always set autoindenting on
  "set smartindent " always set smartindent on

endif " has("autocmd")

" hilite the line with the cursor
set cursorline
hi CursorLine     term=underline 
hi CursorColumn   term=reverse cterm=reverse

" INCLUDE SOME COLOR FOR TT2 DOCS
au BufNewFile,BufRead *.tt2      setf htmlcheetah
au BufRead,BufNewFile *.t set filetype=perl | compiler perlprove

" stuff for perl-support
let g:Perl_AuthorName      = 'Ben Hengst'
let g:Perl_AuthorRef       = ''
let g:Perl_Email           = 'ben@powells.com'
let g:Perl_Company         = 'powells.com'

" set up some perl autocomlete tricks
set iskeyword+=:

" sane colors please
hi Comment cterm=NONE ctermfg=grey "ctermbg=brown
hi Folded  ctermbg=NONE ctermfg=darkgrey
"colorscheme desertEx

" on those rare instances where GVIM is better pick the same font
"set gfn=lime
set guifont=Liberation\ Mono\ 9  

" spellcheking
setlocal spell spelllang=en_us

" get rid of the toolbar in gVim
set guioptions-=T 
" always show the tab bar 
set showtabline=2

" force perl-supports snippits browser to use the inline browser
:filetype plugin on
let g:Perl_GuiSnippetBrowser = 'commandline'
let g:Perl_GuiTemplateBrowser = 'commandline'

" auto compile coffee script
au BufWritePost *.coffee silent CoffeeMake! -bl | cwindow | redraw!

set ruler
" expand tabs to spaces
set tabstop=2
set shiftwidth=2
set expandtab

" vim latex
filetype plugin indent on
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"
"set runtimepath=~/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~/.vim/after


" scrolloff 
set scrolloff=5
