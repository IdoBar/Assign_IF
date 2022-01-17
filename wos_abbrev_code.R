#######################################################################################################################################
# Jabref abbreviations from ISI Web of Science
# Alistair Auffret 
# May 2016 
# This was created using R (the only "programming language" i know), extracting the abbreviations from the web of science lists (https://images.webofknowledge.com/WOKRS520B4.1/help/WOS/A_abrvjt.html) . 
# It is not a perfect list, not least because of the numerous errors in the Web of Science list. 
# However, it was quite a fast way of getting most of the nearly 90 000 journal titles and abbreviations into jab ref.
# Assuming the URL for the abbreviation lists stays the same, you can use the following code in R to create an updated list
# Thanks to Daniel Graeber (dgr@bios.au.dk) for the inspiration to add dots to abbreviated journal names.
#

rm(list=ls())  # clear the workspace

wos.table<-data.frame() #create empty data frame

# loop to create table with all full names and abbreviations from the Web of Science website

for(i in c(LETTERS[c(1:9,11:26)],"0-9")){ # for A-Z plus journals starting with a number. Note that "J" has been removed because the html is different on that page (for some reason, added separately below)
j<-paste("https://images.webofknowledge.com/WOKRS520B4.1/help/WOS/",i,"_abrvjt.html",sep="") # use paste to create URL with the right letters
download.file(j,"/tmp/wostemp",method="curl") # download the file to temp - NOTE: you need to change the temp folder location, or just let them save to working directory and delete them later.
k<-readLines("/tmp/wostemp") # read the temp file you just created
k.table<-data.frame(full=substr(k[grep("<DT>",k)],nchar("<DT>")+1,nchar(as.character(k[grep("<DT>",k)]))),abbrev=substr(k[grep("<B><DD>",k)],nchar("<B><DD>")+1,nchar(as.character(k[grep("<B><DD>",k)]))-nchar(as.character("</B>")))) # makes a table, using grep() to fine the appropriate lines using the html table codes, then using substr() to remove those codes from the table cells.
wos.table<-rbind(wos.table,k.table) # join the table for each letter of the alphabet.
}

# separately for "J".
download.file("https://images.webofknowledge.com/WOKRS520B4.1/help/WOS/J_abrvjt.html","/tmp/wostemp",method="curl") # download the file to temp
k<-readLines("/tmp/wostemp") # read the temp file
k.table<-data.frame(full=substr(k[grep("</B><DT>",k)],nchar("</B><DT>")+1,nchar(as.character(k[grep("</B><DT>",k)]))),abbrev=substr(k[grep("<B><DD>\t",k)],nchar("<B><DD>\t")+1,nchar(as.character(k[grep("<B><DD>\t",k)])))[2:length(grep("<B><DD>\t",k))]) # makes a table, using grep() to fine the appropriate lines using the html table codes, then using substr() to remove those codes from the table cells.
wos.table<-rbind(wos.table,k.table) # join the table to the existing data frame.

wos.table<-wos.table[order(as.character(wos.table$full)),] # alphabetise according to full name (put J back)


# All words in the WoS list are in CAPS, so we use a simple function to capitalise first letter of each word - credit StackOverflow user 'Andrie': http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

    capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s,1,1)),
                      {s <- substring(s,2); if(strict) tolower(s) else s},
                                 sep = "", collapse = " " )
        sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    }




wos.table<-data.frame(full=capwords(tolower(wos.table$full)),abbrev=capwords(tolower(wos.table$abbrev))) # apply that function to create a new version of the table

# not all words should have a leading capital letter, so we need to make lowercase title words lowercase 
lower.words<-c(" In "," For "," And "," To ", " Of ") # make a vector with words usually lowercase in titles. More can be added, but don't forget the space before and after
for(i in lower.words){
wos.table$full<-gsub(i,tolower(i),wos.table$full) # change these to lowercase
}

# Also, we need to replace the html code for "&" for a real &
wos.table$full<-gsub("&amp;","&",wos.table$full)
wos.table$abbrev<-gsub("&amp;","&",wos.table$abbrev)



# The above created a list with abbreviations without full stops after the abbreviated words. Some journal styles require that, so here we create a new column in the table for that.
# This loop took about 15 minutes to run on my computer

for(i in 1:dim(wos.table)[1]){ # for each row in the table
j<- strsplit(as.character(wos.table$full[i])," ") # use strsplit() to make lists of the individual words in the full names
k<- strsplit(as.character(wos.table$abbrev[i])," ") # and the abbreviations

for(l in 1:length(k[[1]])){ # then for each word in each full name
k[[1]][l]<-ifelse(k[[1]][l] %in% j[[1]],k[[1]][l],gsub(k[[1]][l],paste0(k[[1]][l],"."),k[[1]][l])) # use ifelse to see if the word is in the full name. If so, keep it (it's not an abbreviation), if not then replace the word with gsub() to add a full stop after that word.
}
wos.table$abbrev.dots[i]<-lapply(k, paste, collapse = " ")[[1]] # then put the words of the abbreviation back together using lapply() and add that to a new column in the table
}


# create tables in jabref format
jabref.wos<-paste(wos.table$full,"=",wos.table$abbrev)
jabref.wos.dots<-paste(wos.table$full,"=",wos.table$abbrev.dots)

# save to disk
write.table(wos.table,"wos_abbrev_table.csv",row.names=FALSE, sep=";")
write.table(jabref.wos,"jabref_wos_abbrev.txt",row.names=FALSE, col.names=FALSE, quote=FALSE)
write.table(jabref.wos.dots,"jabref_wos_abbrev_dots.txt",row.names=FALSE, col.names=FALSE, quote=FALSE)

