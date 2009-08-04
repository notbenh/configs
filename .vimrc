" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2002 Sep 19
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" This is an alternative that also works in block mode, but the deleted
" text is lost and it only works for putting the current register.
"vnoremap p "_dp

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

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

  set autoindent		" always set autoindenting on
  "set smartindent		" always set smartindent on

endif " has("autocmd")

" hilite the line with the cursor
set cursorline
hi CursorLine     term=underline 
hi CursorColumn   term=reverse cterm=reverse



" better cleaner bindings for the code completeion:
inoremap <silent><Esc>      <C-r>=pumvisible()?"\<lt>C-e>":"\<lt>Esc>"<CR>
inoremap <silent><CR>       <C-r>=pumvisible()?"\<lt>C-y>":"\<lt>CR>"<CR>
inoremap <silent><Down>     <C-r>=pumvisible()?"\<lt>C-n>":"\<lt>Down>"<CR>
inoremap <silent><Up>       <C-r>=pumvisible()?"\<lt>C-p>":"\<lt>Up>"<CR>
inoremap <silent><PageDown> <C-r>=pumvisible()?"\<lt>PageDown>\<lt>C-p>\<lt>C-n>":"\<lt>PageDown>"<CR>
inoremap <silent><PageUp>   <C-r>=pumvisible()?"\<lt>PageUp>\<lt>C-p>\<lt>C-n>":"\<lt>PageUp>"<CR>

set paste " set pase by default to allow for nice clean pasteing
set ruler " enable the stuff at the bottom that always tells you where you are.

" INCLUDE SOME COLOR FOR TT2 DOCS
au BufNewFile,BufRead *.tt2      setf htmlcheetah
au BufRead,BufNewFile *.t set filetype=perl | compiler perlprove

" expand tabs to spaces
set tabstop=3
set shiftwidth=3
set expandtab

" cleaner backup/tmp file maanagement.... kinda
set backupdir=~/.backup
set directory=~/.backup

" stuff for perl-support
let g:Perl_AuthorName      = 'Ben Hengst'
let g:Perl_AuthorRef       = ''
let g:Perl_Email           = 'ben@powells.com'
let g:Perl_Company         = 'powells.com'

" set up some perl autocomlete tricks
set iskeyword+=:

" sane colors please
:hi Comment cterm=NONE ctermfg=grey "ctermbg=brown
:hi Folded  ctermbg=NONE ctermfg=darkgrey

" on those rare instances where GVIM is better pick the same font
set gfn=lime

" spellcheking
setlocal spell spelllang=en_us

:colorscheme desertEx

" get rid of the toolbar in gVim
:set guioptions-=T 
" always show the tab bar 
:set showtabline=2
