use v6;

#use lib 'lib';

use Test;
use Template6;

plan 2;

my $t6 = Template6.new;

my $template = '<html>
<head>
<title>Hello [% name %]</title>
</head>
<body>
<h1>Hello [% name %]</h1>
</body>
</html>
';

$t6.add-template: 'hello', $template;

my $wanted = "<html>
<head>
<title>Hello World</title>
</head>
<body>
<h1>Hello World</h1>
</body>
</html>
";

is $t6.process('hello', :name<World>), $wanted, 'String template.';

$wanted = "<html>
<head>
<title>Hello Universe</title>
</head>
<body>
<h1>Hello Universe</h1>
</body>
</html>
";

is $t6.process('hello', :name<Universe>), $wanted, 'Second string template when already compiled.';

