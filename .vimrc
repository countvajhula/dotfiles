  " VIM - .vimrc file (modified from example file provided by VIM)
  "
  " Siddhartha Kasivajhula
  " last modified: Jul 09, 2007
  "
  " To use this file, copy it to
  "     for Unix and OS/2:  ~/.vimrc
  "             for Amiga:  s:.vimrc
  "  for MS-DOS and Win32:  $VIM\_vimrc

  set nocompatible        " Use Vim defaults (much better!)
  set bs=2                " allow backspacing over everything in insert mode
  set ai                  " always set autoindenting on
  set backup              " keep a backup file
  set backupdir=~/.vim/vimbackups
  set viminfo='20,\"50 " read/write a .viminfo file, don't store more
  set ruler               " show position in status bar
                          " than 50 lines of registers
  set ts=4                " tab stops (tab indentation?)
  set sw=4                " shift width for > and < operators
  set linebreak           " (soft-) wrap lines on words
  set scrolloff=2         " scroll with 2 line buffer for continuity
  set showcmd             " show in-progress command in status bar
  set number              " show line numbers
  set breakindent         " wrap lines taking indentation into account
  set showbreak=..        " wrap lines taking indentation into account

  call pathogen#infect() " pathogen plugin to load other plugins

  " change the mapleader from \ to ,
  " let mapleader=","
  " nah, I like it as \

  color koehler   "color scheme in gvim

  """ Misc. plugins config """
  nmap <leader>t :TagbarToggle<CR>
  nmap <leader>n :NERDTreeToggle<CR>
  nmap <leader>u :GundoToggle<CR>
  " tagbar support for groovy
  let g:tagbar_type_groovy = {
      \ 'ctagstype' : 'groovy',
      \ 'kinds'     : [
          \ 'p:package',
          \ 'c:class',
          \ 'i:interface',
          \ 'f:function',
          \ 'v:variables',
      \ ]
  \ }
  " ConqueTerm plugin shortcuts
  nmap <leader>cs :ConqueTerm bash<CR>

  " vimwikis
  let g:vimwiki_list = [{},
              \ {'path': '~/log/planner', 'path_html': ''},
              \ {'path': '~/work/research/wiki', 'path_html': ''},
              \ {'path': '~/log/ferdywiki', 'path_html': ''},
              \ {'path': '~/work/research/mathwiki', 'path_html': ''},
              \ {'path': '~/work/research/buddhwiki', 'path_html': ''},
              \ {'path': '~/log/themanwiki', 'path_html': ''}]
  " 'auto_export':1 automatically exports to HTML on save

  " TeXBox
  let g:LatexBox_viewer = "open -a Skim"

  " Sessions (only 1 session right now, could add more)
  nmap <leader>ss :mksession! $HOME/.vim/sessions/Session.vim<CR>
  nmap <leader>sq :mksession! $HOME/.vim/sessions/Session.vim<CR>:qa!<CR>
  nmap <leader>sr :source $HOME/.vim/sessions/Session.vim<CR>

  "Enable folding
  "some commands:'zi', 'zo', 'zc', 'zC', 'zO'
  set nofen
  set fdl=0

  "Have the mouse enabled all the time:
  "(Disable with ':set mouse=c')
  set mouse=a

  "Enable filetype plugin
  filetype plugin on
  filetype indent on

  "Incremental search
  set incsearch

  "Smart way to move btw. windows
  "from Amir Salihefendic's .vimrc file @ http://amix.dk/
  map <C-j> <C-W>j
  map <C-k> <C-W>k
  map <C-h> <C-W>h
  map <C-l> <C-W>l

  "Remap space, backspace, and enter since they're useless
  "(note conflict with vimwiki)
  nnoremap <space> 10jzz
  nnoremap <backspace> 10kzz
  " alternative to backspace
  nnoremap <C-space> 10kzz
  "CR remap below conflicts with TeXbox
  "nnoremap <CR> G

  " In text files, always limit the width of text to 78 characters
  "autocmd BufRead *.txt set tw=78

  " For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
  " let & guioptions = substitute(& guioptions, "t", "", "g")

  " Don't use Ex mode, use Q for formatting
  map Q gq

  " Switch syntax highlighting on, when the terminal has colors
  " Also switch on highlighting the last used search pattern.
"  if & t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
"  endif

  augroup cprog
    " Remove all cprog autocommands
    au!

    " When starting to edit a file:
    "   For *.c and *.h files set formatting of comments and set C-indenting on.
    "   For other files switch it off.
    "   Don't change the order, it's important that the line with * comes first.
    autocmd BufRead * set formatoptions=tcql nocindent comments&
    autocmd BufRead *.c,*.h,*.cpp,*.java set formatoptions=croql cindent comments=sr:/*,mb:*,el:*/,://
  augroup END

  augroup gzip
    " Remove all gzip autocommands
    au!

    " Enable editing of gzipped files
    "     read: set binary mode before reading the file
    "           uncompress text in buffer after reading
    "    write: compress file after writing
    "   append: uncompress file, append, compress file
    autocmd BufReadPre,FileReadPre        *.gz set bin
    autocmd BufReadPost,FileReadPost      *.gz let ch_save = & ch|set ch=2
    autocmd BufReadPost,FileReadPost      *.gz '[,']!gunzip
    autocmd BufReadPost,FileReadPost      *.gz set nobin
    autocmd BufReadPost,FileReadPost      *.gz let & ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost      *.gz execute ":doautocmd BufReadPost " . expand("%:r")

    autocmd BufWritePost,FileWritePost    *.gz !mv <afile> <afile>:r
    autocmd BufWritePost,FileWritePost    *.gz !gzip <afile>:r

    autocmd FileAppendPre                 *.gz !gunzip <afile>
    autocmd FileAppendPre                 *.gz !mv <afile>:r <afile>
    autocmd FileAppendPost                *.gz !mv <afile> <afile>:r
    autocmd FileAppendPost                *.gz !gzip <afile>:r
  augroup END

  " count number of results in last search
  nmap <leader>c :%s///gn<CR>

  " 'book-like' scrolling (good for wide monitors)
  noremap <silent> <Leader>vs :<C-u>let @z=&so<CR>:set so=0 noscb<CR>:bo vs<CR>Ljzt:setl scb<CR><C-w>p:setl scb<CR>:let &so=@z<CR>

