// ==========================================================
// EVALUARE COMPLETA - Toate functionalitatile interpretorului

// === 1. LITERALI ===
let lit_int = 42;
let lit_float = 3.14;
let lit_bool_t = true;
let lit_bool_f = false;
let lit_char = 'A';
let lit_string = "Salut";

// === 2. OPERATII ARITMETICE ===
let arit_add = 15 + 7;
let arit_sub = 20 - 8;
let arit_mul = 6 * 7;
let arit_div = 100 / 4;
let arit_mod = 17 % 5;
let arit_float = 3.5 + 2.5;

// === 3. COMPARATII ===
let cmp_lt = 5 < 10;
let cmp_le = 5 <= 5;
let cmp_gt = 10 > 5;
let cmp_ge = 7 >= 8;
let cmp_eq = 3 == 3;
let cmp_neq = 3 != 4;

// === 4. OPERATII LOGICE ===
let log_and = true && false;
let log_or = true || false;
let log_not = !true;

// === 5. OPERATORI UNARI ===
let un_neg = -42;
let un_neg_expr = -(10 + 5);

// === 6. PRECEDENTA ===
let prec1 = 2 + 3 * 4;
let prec2 = (2 + 3) * 4;

// === 7. VARIABILE ===
let var_x = 10;
let var_y = var_x;
let var_z = var_x + var_y;

// === 8. ATRIBUIRE CA EXPRESIE ===
let asg_a = 5;
let asg_b = asg_a = 10;

// === 9. FARA INITIALIZARE ===
let uninit;

// === 10. IF-ELSE TRUE ===
let if_t = 0;
if (5 > 3) {
    if_t = 1;
} else {
    if_t = 2;
}

// === 11. IF-ELSE FALSE ===
let if_f = 0;
if (5 < 3) {
    if_f = 1;
} else {
    if_f = 2;
}

// === 12. WHILE SIMPLU ===
let w_i = 0;
let w_sum = 0;
while (w_i < 5) {
    w_sum = w_sum + w_i;
    w_i = w_i + 1;
}

// === 13. WHILE CONDITIE FALSA ===
let w_skip = 100;
while (w_skip < 0) {
    w_skip = 999;
}

// === 14. CONCATENARE STRING ===
let s1 = "Hello";
let s2 = " World";
let s_concat = s1 + s2;
let s_int = "Val: " + 42;

// === 15. FACTORIAL ===
let fact_n = 5;
let fact_result = 1;
while (fact_n > 0) {
    fact_result = fact_result * fact_n;
    fact_n = fact_n - 1;
}

// === 16. PARE/IMPARE ===
let pi_i = 1;
let pi_even = 0;
let pi_odd = 0;
while (pi_i <= 10) {
    if (pi_i % 2 == 0) {
        pi_even = pi_even + pi_i;
    } else {
        pi_odd = pi_odd + pi_i;
    }
    pi_i = pi_i + 1;
}

// === FINAL ===
// Toate variabilele sunt afisate in starea finala