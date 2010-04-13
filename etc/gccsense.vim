if exists('g:loaded_gccsense')
    finish
endif
let g:loaded_gccsense = 1

if !exists('g:gccsenseGCCRecProgram')
    let g:gccsenseGCCRecProgram = 'gccrec'
endif

if !exists('g:gccsenseAutoPCHProgram')
    let g:gccsenseAutoPCHProgram = 'autopch'
endif

if !exists('g:gccsenseCDriver')
    let g:gccsenseCDriver = 'gcc-code-assist'
endif

if !exists('g:gccsenseCPPDriver')
    let g:gccsenseCPPDriver = 'g++-code-assist'
endif

if !exists('g:gccsenseUseAutoPCH')
    let g:gccsenseUseAutoPCH = 1
endif

if !exists('g:gccsenseUseOmniFunc')
    let g:gccsenseUseOmniFunc = 0
endif

" Check vimproc.
let s:is_vimproc = exists('*vimproc#system')

function! s:system(str, ...)"{{{
  return s:is_vimproc ? (a:0 == 0 ? vimproc#system(a:str) : vimproc#system(a:str, join(a:000)))
        \: (a:0 == 0 ? system(a:str) : system(a:str, join(a:000)))
endfunction"}}}

function! GCCSenseComplete(findstart, base)
    if a:findstart
        let cur_text = strpart(getline('.'), 0, col('.') - 1)
        return match(cur_text, '[^\.:>]*$')
    else
        let buf = getline(1, '$')
        let filename = expand('%')
        let tempfile = expand('%:h') . '/.gccsense.' . expand('%:t')
        call writefile(buf, tempfile)

        let gccrec = shellescape(g:gccsenseGCCRecProgram)
        let autopch = ''
        if g:gccsenseUseAutoPCH
            let autopch = '-a ' . shellescape(g:gccsenseAutoPCHProgram)
        endif
        let driver = g:gccsenseCDriver
        if match(expand('%:e'), '\(cpp\|cc\|cxx\|CPP\|CC\|CXX\)') >= 0
            let driver = g:gccsenseCPPDriver
        endif
        let command = printf('%s -r %s -d %s -a "%s" "%s" -fsyntax-only "-code-completion-at=%s:%s:%s"',
                             \ gccrec,
                             \ autopch,
                             \ driver,
                             \ tempfile,
                             \ filename,
                             \ tempfile,
                             \ line('.'),
                             \ col('.') + 1)
        let result = split(s:system(command), "\n")
        call delete(tempfile)
        let completions = []
        for item in result
            if item =~ '^completion: ' . a:base
                call add(completions, split(item, ' ')[1])
            endif
        endfor
        return completions
    endif
endfunction

function! SetupGCCSense()
    if g:gccsenseUseOmniFunc
        setlocal omnifunc=GCCSenseComplete
    else
        setlocal completefunc=GCCSenseComplete
    endif
endfunction

function! s:GCCSenseDiagnoseProgram(program, args, should_match, msg)
    let success = 0
    if s:system(shellescape(a:program) . ' ' . a:args) =~ a:should_match
        let success = 1
    endif
    if !success
        echo 'Error: ' . printf(a:msg, a:program)
    end
endfunction

function! GCCSenseDiagnose()
    call s:GCCSenseDiagnoseProgram(g:gccsenseGCCRecProgram, '--version', '.', "`%s' is not executable from Vim or return error. Please make sure that the program was installed correctly.")
    call s:GCCSenseDiagnoseProgram(g:gccsenseAutoPCHProgram, '--version', '.', "`%s' is not executable from Vim or return error. Please make sure that the program was installed correctly.")
    call s:GCCSenseDiagnoseProgram(g:gccsenseCDriver, '--version', '.', "`%s' is not executable from Vim or return error. Please make sure that the program was installed correctly.")
    call s:GCCSenseDiagnoseProgram(g:gccsenseCPPDriver, '--version', '.', "`%s' is not executable from Vim or return error. Please make sure that the program was installed correctly.")
    call s:GCCSenseDiagnoseProgram(g:gccsenseCDriver, '-code-completion-at=x', 'no input file', "`%s' can not take `-code-completion-at' option. Make sure that gcc-code-assist was installed correctly and g:gccsenseCDriver points to that programs.")
    call s:GCCSenseDiagnoseProgram(g:gccsenseCPPDriver, '-code-completion-at=x', 'no input file', "`%s' can not take `-code-completion-at' option. Make sure that g++-code-assist was installed correctly and g:gccsenseCPPDriver points to that programs.")

    echo "Finish!"
endfunction

autocmd FileType c call SetupGCCSense()
autocmd FileType cpp call SetupGCCSense()
