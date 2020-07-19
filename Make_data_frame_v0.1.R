library('readr')
library('tidyr')
library('dplyr')
library('stringr')

# Declare function --------------------------------------------------------
make_subtitle_df <- function(movie_name,
                              data_path = str_interp('${getwd()}/data/'),
                              result_path = str_interp('${getwd()}/result/'),
                              return_result = TRUE,
                              write_file = TRUE){
  
  file_name <- str_interp('${data_path}/${movie_name}_ko.srt')
  
  guessed_encoding <- readr::guess_encoding(file_name)
  subtitle_raw <- readLines(file_name, encoding = guessed_encoding$encoding[1] )
  
  # 공백 제거
  subtitle_raw <- subtitle_raw[subtitle_raw != '']
  
  # 상황설명 분리
  #situation <- subtitle_raw[grepl('\\[|\\]'  , subtitle_raw)]
  subtitle_raw <- subtitle_raw[!grepl('\\[|\\]', subtitle_raw)]
  
  sub_idx_loc <- grep('^[0-9]*[0-9]$', subtitle_raw)
  sub_idx <- rep(NA_character_, length(subtitle_raw))
  
  sub_idx[sub_idx_loc] <- sub_idx_loc
  
  subtitle_df <- data.frame(subtitle = subtitle_raw,
                             sub_idx = as.numeric(sub_idx),
                             stringsAsFactors = FALSE)
  
  subtitle_df <- subtitle_df %>%
    fill(sub_idx)
  
  # 대사 번호, 대사 구간
  idx_col <- subtitle_df %>% 
    group_by(sub_idx) %>%
    slice(1)%>%
    rename('idx'='subtitle')
  
  time_col <- subtitle_df %>% 
    group_by(sub_idx) %>%
    slice(2)%>%
    rename('timestamp'='subtitle')
  
  text_col <- subtitle_df %>% 
    group_by(sub_idx) %>%
    slice(-(1:2)) %>%
    summarise_all(funs(paste(., collapse = ' '))) %>%
    rename('script'='subtitle')
  
  sub_df <- idx_col %>%
    full_join(time_col, by = 'sub_idx') %>%
    full_join(text_col, by = 'sub_idx') %>% 
    ungroup %>%
    select(-sub_idx)
  
  # 다수의 발화자 컬럼에서 leading - 삭제
  # row 중간에 발화자가 바뀌는 경우 -가 있음
  sub_df <- sub_df %>%
    mutate(script = gsub('^- ', '',script))
  
  # row당 발화자가 1명이도록 처리
  # 주민등록번호, 전화번호등이 대사에 있는 경우가 있다. -> 분리자 뒤에 스페이스를 추가
  
  sub_df <- sub_df %>% 
    separate_rows(script, sep = "- ") %>%
    mutate(script = trimws(script, 'left'))
  
  # 발화자 컬럼 추가, # 다수의 발화자가 있다
  sub_df <- sub_df %>%
    mutate( speaker = ifelse(grepl('^\\(', script),
                             str_sub(script, 2, str_locate(script, '\\)')[,1] -1),
                             NA))
  
  # 원본 script 에서 발화자 표기 제거
  sub_df <- sub_df %>%
    mutate(script = trimws(gsub('\\([^)]*\\)', '',script), 'left'))
  
  # script가 NA 것 제거
  sub_df <- sub_df %>%
    filter(!is.na(script))
  
  # timestamp를 fromm, to로 분리
  sub_df <- sub_df %>%
    mutate(time_from = str_sub(timestamp, 1, str_locate(timestamp, ' --> ')[,1] - 1),
           time_to = str_sub(timestamp, str_locate(timestamp, ' --> ')[,1] + 5, nchar(timestamp))) %>%
    select(-timestamp)

  if(write_file){
    
    if(!dir.exists(result_path)){
      dir.create(result_path)
    }
    
    write.csv(sub_df, str_interp('${result_path}/${movie_name}.csv') )
    
  }
  
  if(return_result) return(sub_df)

}

# Run Code ----------------------------------------------------------------
# Set path
data_path <- str_interp('${getwd()}/data/') # srt파일들이 위치한 경로 
result_path <- str_interp('${getwd()}/result/') # csv생성시 저장 경로

# Get file list
# 파일명은 {movie_name}_ko.srt 여야함
movie_list <- gsub('_ko.srt', '', list.files(data_path))

# Make csv files
for(mv_name in movie_list ){

  tryCatch({
    make_subtitle_df(mv_name,
                         return_result = FALSE)
    },
    error = function(e){
      print(str_interp('${mv_name} : ${e}'))
    })
  
}

