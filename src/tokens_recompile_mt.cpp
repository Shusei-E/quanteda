//#include "dev.h"
#include "lib.h"
#include "recompile.h"
using namespace quanteda;

/* 
* This function recompiles tokens object.
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types in tokens
* @param gap if TRUE, remove gaps between token IDs
* @param dup if TRUE, merge duplicated token types into the same ID 
*/

// [[Rcpp::export]]
List qatd_cpp_tokens_recompile(const List &texts_, 
                               const CharacterVector types_,
                               const bool gap = true,
                               const bool dup = true){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    return recompile(texts, types, gap, dup);
    
}


// [[Rcpp::export]]
List qatd_cpp_tokens_remap(const List &texts_,
                           const CharacterVector types_,
                           const IntegerVector ids_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    
    if (types_.size() == ids_.size()) {
        throw std::range_error("Invalid IDs");
    }
    
    VecIds ids_new(types.size() + 1);
    ids_new[0] = 0; // reserved for padding
    for (std::size_t g = 0; g < ids_.size(); g++) {
        ids_new[g + 1] = ids_[g];
    }
    
#if QUANTEDA_USE_TBB
    recompile_mt recompile_mt(texts, ids_new);
    parallelFor(0, texts.size(), recompile_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            if (texts[h][i] >= ids_new.size()) {
                throw std::range_error("Invalid tokens object");
            }
            texts[h][i] = ids_new[texts[h][i]];
        }
    }
#endif
    
    Tokens texts_new_ = Rcpp::wrap(texts);
    texts_new_.attr("types") = types_;
    texts_new_.attr("padding") = texts_.attr("padding");
    texts_new_.attr("class") = "tokens";
    return texts_new_;
}


/***R

#toks3 <- list(rep(0:5, 1), rep(10:15, 1))
toks3 <- list(0:26)
qatd_cpp_tokens_recompile(toks3, letters)

toks4 <- list(c(1:5))
qatd_cpp_tokens_recompile(toks4, c('あ', 'い', 'う', 'え', 'お'))



*/
