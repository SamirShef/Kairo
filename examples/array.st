func int main()
{
    var int[] numbers = [1, 2, 3, 4];
    var int[] numbers2 = [4, 3, 2, 1];

    numbers = numbers2;
    numbers2 = [5, 6, 7, 8];
    
    return 0;
}