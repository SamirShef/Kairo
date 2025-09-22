const double PI = 3.141592653589793238462643383279502884d;
const double E = 2.718281828459045235360287471352662498d;

func double abs(double x)
{
    if (x >= 0) return x;
    return -x;
}

func double min(double a, double b)
{
    if (a <= b) return a;
    return b;
}

func double max(double a, double b)
{
    if (a >= b) return a;
    return b;
}

func double ln(double x)
{
    if (x <= 0) return 0;
    
    var double result = 0d;
    var int n = 0;
    
    while (x > 1.5d)
    {
        x /= E;
        n++;
    }
    while (x < 0.5d)
    {
        x *= E;
        n--;
    }
    
    var double u = x - 1d;
    var double u2 = u * u;
    var double u3 = u2 * u;
    var double u4 = u3 * u;
    var double u5 = u4 * u;
    var double u6 = u5 * u;
    var double u7 = u6 * u;
    var double u8 = u7 * u;
    var double u9 = u8 * u;
    var double u10 = u9 * u;
    var double u11 = u10 * u;
    var double u12 = u11 * u;
    var double u13 = u12 * u;
    var double u14 = u13 * u;
    var double u15 = u14 * u;
    var double u16 = u15 * u;
    var double u17 = u16 * u;
    var double u18 = u17 * u;
    var double u19 = u18 * u;
    var double u20 = u19 * u;
    var double u21 = u20 * u;
    var double u22 = u21 * u;
    var double u23 = u22 * u;
    var double u24 = u23 * u;
    var double u25 = u24 * u;
    var double u26 = u25 * u;
    var double u27 = u26 * u;
    var double u28 = u27 * u;
    var double u29 = u28 * u;
    var double u30 = u29 * u;
    var double u31 = u30 * u;
    var double u32 = u31 * u;
    var double u33 = u32 * u;
    var double u34 = u33 * u;
    var double u35 = u34 * u;
    var double u36 = u35 * u;
    var double u37 = u36 * u;
    var double u38 = u37 * u;
    var double u39 = u38 * u;
    var double u40 = u39 * u;
    var double u41 = u40 * u;
    var double u42 = u41 * u;
    var double u43 = u42 * u;
    var double u44 = u43 * u;
    var double u45 = u44 * u;
    var double u46 = u45 * u;
    var double u47 = u46 * u;
    var double u48 = u47 * u;
    var double u49 = u48 * u;
    var double u50 = u49 * u;
    var double u51 = u50 * u;
    var double u52 = u51 * u;
    var double u53 = u52 * u;
    var double u54 = u53 * u;
    var double u55 = u54 * u;
    var double u56 = u55 * u;
    var double u57 = u56 * u;
    var double u58 = u57 * u;
    var double u59 = u58 * u;
    var double u60 = u59 * u;
    var double u61 = u60 * u;
    var double u62 = u61 * u;
    var double u63 = u62 * u;
    var double u64 = u63 * u;
    var double u65 = u64 * u;
    var double u66 = u65 * u;
    var double u67 = u66 * u;
    var double u68 = u67 * u;
    var double u69 = u68 * u;
    var double u70 = u69 * u;
    var double u71 = u70 * u;
    var double u72 = u71 * u;
    var double u73 = u72 * u;
    var double u74 = u73 * u;
    var double u75 = u74 * u;
    var double u76 = u75 * u;
    var double u77 = u76 * u;
    var double u78 = u77 * u;
    var double u79 = u78 * u;
    var double u80 = u79 * u;
    
    result = u - u2/2d + u3/3d - u4/4d + u5/5d - u6/6d + u7/7d - u8/8d + u9/9d - u10/10d + u11/11d - u12/12d + u13/13d - u14/14d + u15/15d - u16/16d + u17/17d - u18/18d + u19/19d - u20/20d + u21/21d - u22/22d + u23/23d - u24/24d + u25/25d - u26/26d + u27/27d - u28/28d + u29/29d - u30/30d + u31/31d - u32/32d + u33/33d - u34/34d + u35/35d - u36/36d + u37/37d - u38/38d + u39/39d - u40/40d + u41/41d - u42/42d + u43/43d - u44/44d + u45/45d - u46/46d + u47/47d - u48/48d + u49/49d - u50/50d + u51/51d - u52/52d + u53/53d - u54/54d + u55/55d - u56/56d + u57/57d - u58/58d + u59/59d - u60/60d + u61/61d - u62/62d + u63/63d - u64/64d + u65/65d - u66/66d + u67/67d - u68/68d + u69/69d - u70/70d + u71/71d - u72/72d + u73/73d - u74/74d + u75/75d - u76/76d + u77/77d - u78/78d + u79/79d - u80/80d;
    
    return result + n;
}

func double exp(double x)
{
    var bool isNegative = x < 0d;
    if (isNegative) x = -x;
    
    var double result = 1d;
    var int n = 0;
    
    while (x > 1d)
    {
        x /= 2d;
        n++;
    }
    
    var double term = 1d;
    
    for (int i = 1; i <= 50; i++)
    {
        term *= x / i;
        result += term;
    }
    
    for (int i = 0; i < n; i++) result *= result;
    
    if (isNegative) return 1d / result;
    return result;
}

func double pow(double x, int power)
{
    if (power == 0) return 1;

    var bool isNegativePower = power < 0;
    var double res = 1;
    for (int i; i < abs(power); i++) res *= x;

    if (isNegativePower) return 1 / res;
    return res;
}

func double sqrt(double x)
{
    if (x < 0d) return 0d;
    if (x == 0d) return 0d;
    if (x == 1d) return 1d;
    
    if (x < 0.000000000000001d) return 0d;
    
    var int scale = 0;
    while (x > 10000000000d)
    {
        x /= 10000000000d;
        scale += 5; // 1e10 = (1e5)^2
    }
    while (x < 0.0000000001d)
    {
        x *= 10000000000d;
        scale -= 5;
    }
    
    var double guess;
    if (x > 1d)
    {
        var int exp = 0;
        var double temp = x;
        while (temp >= 2d)
        {
            temp /= 2d;
            exp++;
        }
        guess = 1d;
        for (int i = 0; i < exp / 2; i++) guess *= 2d;
        if (exp % 2 == 1) guess *= 1.4142135623730951d; // √2
    }
    else guess = 0.5d + 0.5d * x;
    
    for (int i = 0; i < 15; i++)
    {
        var double prev = guess;
        guess = (guess + x / guess) / 2d;
        
        var double diff = guess - prev;
        if (diff < 0d) diff = -diff;
        if (diff < 0.000000000000001d) break;
    }
    
    if (scale > 0)
        for (int i = 0; i < scale; i++)
            guess *= 100000d;
    else if (scale < 0)
        for (int i = 0; i < -scale; i++)
            guess /= 100000d;
    
    return guess;
}

func double root(double x, double n)
{
    var int nInt = n;
    if (x < 0d && nInt % 2 == 0) return 0d;
    if (x == 0d) return 0d;
    if (n == 0d) return 1d;
    if (n == 1d) return x;
    if (n == 2d) return sqrt(x);
    
    return exp(ln(abs(x)) / n);
}

func double cbrt(double x)
{
    if (x == 0d) return 0d;
    
    var bool isNegative = x < 0d;
    if (isNegative) x = -x;
    
    var double guess = x;
    if (x > 1d)
    {
        var int exp = 0;
        var double temp = x;
        while (temp >= 8d) // 2^3 = 8
        {
            temp /= 8d;
            exp++;
        }
        guess = 1d;
        for (int i = 0; i < exp; i++) guess *= 2d;
    }
    else guess = 0.5d + 0.5d * x;
    
    for (int i = 0; i < 15; i++)
    {
        var double prev = guess;
        var double guess2 = guess * guess;
        guess = (2d * guess + x / guess2) / 3d;
        
        var double diff = guess - prev;
        if (diff < 0d) diff = -diff;
        if (diff < 0.000000000000001d) break;
    }
    
    if (isNegative) return -guess;
    return guess;
}

func double pow(double x, double power)
{
    if (x == 0d)
    {
        if (power > 0d) return 0d;
        if (power == 0d) return 1d;
        return 0d;
    }
    
    if (x < 0d) return 0d;
    
    if (power == 0d) return 1d;
    if (power == 1d) return x;
    if (power == -1d) return 1d / x;
    
    return exp(power * ln(x));
}

func int round(double x)
{
    var int negativeMult = 1;
    if (x < 0) negativeMult = -1;
    var int intPart = abs(x);
    if (abs(x) - intPart >= 0.5d) return (intPart + 1) * negativeMult;
    return intPart * negativeMult;
}

func int floor(double x)
{
    var bool isNegative = x < 0;
    var int intPart = abs(x);
    if (isNegative) return -intPart - 1;
    return intPart;
}

func int ceil(double x)
{
    var bool isNegative = x < 0;
    var int intPart = abs(x);
    if (isNegative) return -intPart;
    return intPart + 1;
}

func double factorial(int n)
{
    if (n <= 1) return 1d;
    var double result = 1d;
    for (int i = 2; i <= n; i++) result *= i;
    return result;
}

func double radToDeg(double x)
{
    return x * 180d / PI;
}

func double degToRad(double x)
{
    return x * PI / 180d;
}

func double sin(double x)
{
    while (x > 2d * PI) x -= 2d * PI;
    while (x < -2d * PI) x += 2d * PI;
    
    if (x > PI) x -= 2d * PI;
    if (x < -PI) x += 2d * PI;
    
    var bool isNegative = x < 0;
    if (isNegative) x = -x;
    
    if (x < 0.0001d) 
    {
        if (isNegative) return -x;
        return x;
    }
    
    var double x2 = x * x;                                                                                          // x^2
    var double x3 = x2 * x;                                                                                         // x^3
    var double x5 = x3 * x2;                                                                                        // x^5
    var double x7 = x5 * x2;                                                                                        // x^7
    var double x9 = x7 * x2;                                                                                        // x^9
    var double x11 = x9 * x2;                                                                                       // x^11
    var double x13 = x11 * x2;                                                                                      // x^13
    var double x15 = x13 * x2;                                                                                      // x^15
    var double x17 = x15 * x2;                                                                                      // x^17
    var double x19 = x17 * x2;                                                                                      // x^19
    var double x21 = x19 * x2;                                                                                      // x^21
    var double x23 = x21 * x2;                                                                                      // x^23
    var double x25 = x23 * x2;                                                                                      // x^25
    var double x27 = x25 * x2;                                                                                      // x^27
    var double x29 = x27 * x2;                                                                                      // x^29
    var double x31 = x29 * x2;                                                                                      // x^31
    var double x33 = x31 * x2;                                                                                      // x^33
    var double x35 = x33 * x2;                                                                                      // x^35
    var double x37 = x35 * x2;                                                                                      // x^37
    var double x39 = x37 * x2;                                                                                      // x^39
    var double x41 = x39 * x2;                                                                                      // x^41
    var double x43 = x41 * x2;                                                                                      // x^43
    var double x45 = x43 * x2;                                                                                      // x^45
     
    const double c1 = 1d;                                                                                           // 1/1!
    const double c3 = -1d / 6d;                                                                                     // -1/3!
    const double c5 = 1d / 120d;                                                                                    // 1/5!
    const double c7 = -1d / 5040d;                                                                                  // -1/7!
    const double c9 = 1d / 362880d;                                                                                 // 1/9!
    const double c11 = -1d / 39916800d;                                                                             // -1/11!
    const double c13 = 1d / 6227020800d;                                                                            // 1/13!
    const double c15 = -1d / 1307674368000d;                                                                        // -1/15!
    const double c17 = 1d / 355687428096000d;                                                                       // 1/17!
    const double c19 = -1d / 121645100408832000d;                                                                   // -1/19!
    const double c21 = 1d / 51090942171709440000d;                                                                  // 1/21!
    const double c23 = -1d / 25852016738884976640000d;                                                              // -1/23!
    const double c25 = 1d / 15511210043330985984000000d;                                                            // 1/25!
    const double c27 = -1d / 8841761993739701954543616000000d;                                                      // -1/27!
    const double c29 = 1d / 822283865417792281772556288000000d;                                                     // 1/29!
    const double c31 = -1d / 8683317618811886495518194401280000000d;                                                // -1/31!
    const double c33 = 1d / 103331479663861449296666513375232000000000d;                                            // 1/33!
    const double c35 = -1d / 13763753091226345046315979581580902400000000000d;                                      // -1/35!
    const double c37 = 1d / 1982608315404440064116146708361898134400000000000d;                                     // 1/37!
    const double c39 = -1d / 310224200866619719680000000000000000000000000000000d;                                  // -1/39!
    const double c41 = 1d / 507580213877224798800856812176625227226004528988036000000000000000000000d;              // 1/41!
    const double c43 = -1d / 884176199373970195454361600000000000000000000000000000000000000000000000000d;          // -1/43!
    const double c45 = 1d / 160504532577688183142302981490463987596544700000000000000000000000000000000000000d;     // 1/45!
     
    var double result = c1 * x + c3 * x3 + c5 * x5 + c7 * x7 + c9 * x9 + c11 * x11 + c13 * x13 + c15 * x15 + c17 * x17 + c19 * x19 + c21 * x21 + c23 * x23 + c25 * x25 + c27 * x27 + c29 * x29 + c31 * x31 + c33 * x33 + c35 * x35 + c37 * x37 + c39 * x39 + c41 * x41 + c43 * x43 + c45 * x45;
    
    if (isNegative) return -result;
    return result;
}

func double cos(double x)
{
    return sin(x + PI / 2d);
}

func double tan(double x)
{
    var double cosx = cos(x);
    if (abs(cosx) < 0.0000000001d) return 0d;
    return sin(x) / cosx;
}