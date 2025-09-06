func test()
{
    echo "Hello world\n";
}

func test(int a)
{
    echo a * a;
    echo '\n';
}

func int main()
{
    test();
    test(42);

    return 0;
}