## 

for(ii in seq_len(nrow(tw_df))) {
  
  if(is.na(tw_df$img_source[ii]) == F) {
    
    download.file(tw_df$img_source[ii],
                  destfile = paste0("ponta_image/pi_", tw_df$id[ii], ".jpeg"),
                  mode = "wb")
}
  
  message(ii)
}
  
