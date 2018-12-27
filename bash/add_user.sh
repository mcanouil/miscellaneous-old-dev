#!/bin/bash

USER=$1
PWD=$2
ID=$3

mkdir /home/$USER \
&& useradd -u $ID $USER --home /home/$USER \
&& echo "$USER:$PWD" | chpasswd \
&& chown -R $USER:staff /home/$USER \
&& addgroup $USER staff \
&& addgroup $USER root \
&& adduser $USER sudo \
&& usermod -g staff $USER \
&& mkdir -p /home/$USER/.rstudio/monitored/user-settings \
&& [ -f /home/$USER/.rstudio/monitored/user-settings/user-settings ] || echo 'alwaysSaveHistory="0" 
cleanTexi2DviOutput="1" 
cleanupAfterRCmdCheck="1" 
cranMirrorCountry="us" 
cranMirrorHost="RStudio" 
cranMirrorName="Global (CDN)" 
cranMirrorUrl="http://cran.rstudio.com/" 
customShellCommand="" 
customShellOptions="" 
defaultTerminalShell="7" 
enableLaTeXShellEscape="1" 
errorHandlerType="1" 
hideObjectFiles="1" 
lineEndingConversion="1" 
loadRData="0" 
newlineInMakefiles="1" 
removeHistoryDuplicates="0" 
restoreLastProject="0" 
reuseSessionsForProjectLinks="1" 
rprofileOnResume="0" 
saveAction="0" 
securePackageDownload="1" 
showLastDotValue="0" 
showUserHomePage="sessions" 
uiPrefs="{\\n    \"always_complete_characters\" : 3,\\n    \"always_complete_console\" : true,\\n    \"always_complete_delay\" : 250,\\n    \"always_enable_concordance\" : true,\\n    \"ansi_console_mode\" : 1,\\n    \"auto_append_newline\" : true,\\n    \"auto_expand_error_tracebacks\" : false,\\n    \"auto_run_setup_chunk\" : true,\\n    \"background_diagnostics_delay_ms\" : 2000,\\n    \"blinking_cursor\" : true,\\n    \"busy_detection\" : 0,\\n    \"busy_whitelist\" : [\\n        \"tmux\",\\n        \"screen\"\\n    ],\\n    \"check_arguments_to_r_function_calls\" : false,\\n    \"clear_hidden\" : false,\\n    \"code_complete\" : \"manual\",\\n    \"code_complete_other\" : \"manual\",\\n    \"continue_comments_on_newline\" : false,\\n    \"default_encoding\" : \"UTF-8\",\\n    \"default_latex_program\" : \"pdfLaTeX\",\\n    \"default_project_location\" : \"~\",\\n    \"default_sweave_engine\" : \"knitr\",\\n    \"diagnostics_in_function_calls\" : true,\\n    \"diagnostics_on_save\" : true,\\n    \"doc_outline_show\" : \"show_sections_only\",\\n    \"enable_background_diagnostics\" : true,\\n    \"enable_emacs_keybindings\" : false,\\n    \"enable_rsconnect_publish_ui\" : true,\\n    \"enable_snippets\" : true,\\n    \"enable_style_diagnostics\" : false,\\n    \"execution_behavior\" : \"statement\",\\n    \"flat_theme\" : \"default\",\\n    \"focus_console_after_exec\" : false,\\n    \"fold_style\" : \"markbegin\",\\n    \"font_size_points\" : 10,\\n    \"git_diff_ignore_whitespace\" : false,\\n    \"handle_errors_in_user_code_only\" : true,\\n    \"hide_console_on_chunk_execute\" : true,\\n    \"highlight_code_chunks\" : true,\\n    \"highlight_r_function_calls\" : false,\\n    \"highlight_selected_line\" : false,\\n    \"highlight_selected_word\" : true,\\n    \"ignore_uppercase_words\" : true,\\n    \"ignore_words_with_numbers\" : true,\\n    \"insert_matching\" : false,\\n    \"insert_numbered_latex_sections\" : true,\\n    \"insert_parens_after_function_completion\" : false,\\n    \"insert_spaces_around_equals\" : true,\\n    \"latex_preview_on_cursor_idle\" : \"always\",\\n    \"navigate_to_build_error\" : true,\\n    \"num_spaces_for_tab\" : 2,\\n    \"packages_pane_enabled\" : true,\\n    \"pane_config\" : {\\n        \"consoleLeftOnTop\" : false,\\n        \"consoleRightOnTop\" : true,\\n        \"panes\" : [\\n            \"Source\",\\n            \"TabSet1\",\\n            \"Console\",\\n            \"TabSet2\"\\n        ],\\n        \"tabSet1\" : [\\n            \"Environment\",\\n            \"History\",\\n            \"Connections\",\\n            \"VCS\",\\n            \"Presentation\"\\n        ],\\n        \"tabSet2\" : [\\n            \"Files\",\\n            \"Plots\",\\n            \"Packages\",\\n            \"Help\",\\n            \"Build\",\\n            \"Viewer\"\\n        ]\\n    },\\n    \"pdf_previewer\" : \"rstudio\",\\n    \"preferred_document_outline_width\" : 110,\\n    \"print_margin_column\" : 80,\\n    \"reindent_on_paste\" : false,\\n    \"restore_source_documents\" : false,\\n    \"rmd_chunk_output_inline\" : true,\\n    \"rmd_preferred_template_path\" : \"\",\\n    \"rmd_viewer_type\" : 1,\\n    \"root_document\" : \"\",\\n    \"save_before_sourcing\" : true,\\n    \"save_files_before_build\" : false,\\n    \"scroll_past_end_of_document\" : false,\\n    \"show_diagnostics_cpp\" : true,\\n    \"show_diagnostics_other\" : true,\\n    \"show_diagnostics_r\" : true,\\n    \"show_doc_outline_rmd\" : false,\\n    \"show_help_tooltip_on_idle\" : false,\\n    \"show_indent_guides\" : true,\\n    \"show_inline_toolbar_for_r_code_chunks\" : true,\\n    \"show_invisibles\" : false,\\n    \"show_line_numbers\" : true,\\n    \"show_margin\" : false,\\n    \"show_publish_diagnostics\" : false,\\n    \"show_publish_ui\" : false,\\n    \"show_signature_tooltips\" : false,\\n    \"soft_wrap_r_files\" : false,\\n    \"source_with_echo\" : false,\\n    \"spelling_dictionary_language\" : \"en_GB\",\\n    \"strip_trailing_whitespace\" : false,\\n    \"surround_selection\" : \"never\",\\n    \"syntax_color_console\" : false,\\n    \"tab_multiline_completion\" : false,\\n    \"terminal_autoclose\" : true,\\n    \"terminal_local_echo\" : true,\\n    \"terminal_track_env\" : true,\\n    \"terminal_websockets\" : false,\\n    \"theme\" : \"Cobalt\",\\n    \"toolbar_visible\" : true,\\n    \"truncate_long_lines_in_console\" : 1000,\\n    \"use_dataimport\" : true,\\n    \"use_rcpp_template\" : true,\\n    \"use_roxygen\" : false,\\n    \"use_spaces_for_tab\" : true,\\n    \"use_vim_mode\" : false,\\n    \"valign_argument_indent\" : true,\\n    \"warn_if_no_such_variable_in_scope\" : false,\\n    \"warn_if_variable_defined_but_not_used\" : false,\\n    \"wrap_tab_navigation\" : false\\n}" 
useDevtools="1" 
useInternet2="1"' > /home/$USER/.rstudio/monitored/user-settings/user-settings \
&& echo 'R_MAX_NUM_DLLS=300' > /home/$USER/.Renviron \
&& [ -f /home/$USER/.bashrc ] || echo '
# User specific aliases and functions
### set locales
export LANG="en_GB.UTF-8"
export LANGUAGE="en_GB.UTF-8"
export LC_CTYPE="en_GB.UTF-8"
export LC_NUMERIC="en_GB.UTF-8"
export LC_TIME="en_GB.UTF-8"
export LC_COLLATE="en_GB.UTF-8"
export LC_MONETARY="en_GB.UTF-8"
export LC_MESSAGES="en_GB.UTF-8"
export LC_PAPER="en_GB.UTF-8"
export LC_NAME="en_GB.UTF-8"
export LC_ADDRESS="en_GB.UTF-8"
export LC_TELEPHONE="en_GB.UTF-8"
export LC_MEASUREMENT="en_GB.UTF-8"
export LC_IDENTIFICATION="en_GB.UTF-8"
export LC_ALL="en_GB.UTF-8"

alias R="R --no-save --no-restore-data"

alias gaa="git add --all"
alias gam="git commit -am"
alias gm="git commit -m"
alias gp="git push"
alias gss="git status"

alias cp="cp -iv"                           # Nouvelle copie
alias mv="mv -iv"                           # Nouveau move
alias mkdir="mkdir -pv"                     # Nouvelle création de dossier
alias ll="ls -FGlAhp --color=auto"               # Affiche fichier, dossier, et fichiers cachés
alias l="ls -FGlhp --color=auto"                 # Affiche fichier et dossier
alias ls="ls --color=auto" 
cd() { builtin cd "$@"; ll; }               # Changement de dossier
alias cd..="cd ../"                         # Retour en arrière rapide
alias ..="cd ../"                           # Retour rapide 1 niveau
alias ...="cd ../../"                       # Retour rapide 2 niveaux
alias .3="cd ../../../"                     # Retour rapide 3 niveaux
alias .4="cd ../../../../"                  # Retour rapide 4 niveaux
alias .5="cd ../../../../../"               # Retour rapide 5 niveaux
' > /home/$USER/.bashrc \
&& [ -f /home/$USER/.bash_profile ] || echo '
# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
' > /home/$USER/.bash_profile \
&& chown -R $USER:staff /home/$USER
