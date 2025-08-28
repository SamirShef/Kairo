func test()
{
    echo "Hello world";
}

func test(int a)
{
    echo a * a;
}

func int main()
{
    test();
    test(42);

    return 0;
}