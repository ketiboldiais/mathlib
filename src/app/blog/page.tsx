import { getLatestPostSummaries } from "@/utils/mdxUtils";
import Link from "next/link";

const BlogIndexPage = async () => {
    const posts = await getLatestPostSummaries();
    return (
        <div>
            <h1>Posts</h1>
            <ul>
                {posts.map((post) => (
                    <li key={post.slug}>
                        <Link href={`/blog/${post.slug}`}>
                            <h2>{post.title}</h2>
                            <p>{post.description}</p>
                            <p>{new Date(post.date).toDateString()}</p>
                        </Link>
                    </li>
                ))}
            </ul>
        </div>
    )
}

export default BlogIndexPage;